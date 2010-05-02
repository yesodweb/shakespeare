{-# LANGUAGE TemplateHaskell #-}
module Text.Hamlet.Quasi
    ( hamlet
    , xhamlet
    , hamletWithSettings
    ) where

import Text.Hamlet.Parse
import Text.Hamlet.Monad
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Control.Monad
import Data.List (sortBy, isPrefixOf)

type Vars = (Scope, Exp, [Stmt] -> [Stmt])

type Scope = [([Ident], Exp)]

docsToExp :: [Doc] -> Q Exp
docsToExp docs = do
    arg <- newName "_arg"
    (_, _, stmts) <- foldM docToStmt ([], VarE arg, id) docs
    stmts' <- case stmts [] of
                    [] -> do
                        ret <- [|return ()|]
                        return [NoBindS ret]
                    x -> return x
    return $ LamE [VarP arg] $ DoE stmts'

docToStmt :: Vars -> Doc -> Q Vars
docToStmt (vars, arg, stmts) (DocForall (Deref idents) ident@(Ident name) inside) = do
    (vars', deref', stmts', isEnum) <-
        case idents of
            [] -> error $ "Invalid forall deref: " ++ show idents
            [(x, Ident i)] -> do
                let i' = VarE (mkName i) `AppE` arg
                return (vars, i', id, x)
            _ -> do
                let front = Deref $ init idents
                    (x, Ident i) = last idents
                (vars', base, stmts') <- bindDeref vars arg front
                return (vars', VarE (mkName i) `AppE` base, stmts', x)
    fl <- [|fromList|]
    let deref'' = if isEnum then deref' else fl `AppE` deref'
    mh <- [|mapH|]
    ident' <- newName name
    let vars'' = ([ident], VarE ident') : vars'
    (_, _, inside') <- foldM docToStmt (vars'', arg, id) inside
    let dos = LamE [VarP ident'] $ DoE $ inside' []
    let stmt = NoBindS $ mh `AppE` dos `AppE` deref''
    return (vars', arg, stmts . stmts' . (:) stmt)
docToStmt (vars, arg, stmts) (DocCond conds final) = do
    conds' <- liftConds vars arg conds id
    final' <- case final of
                Nothing -> [|Nothing|]
                Just f -> do
                    (_, _, f') <- foldM docToStmt (vars, arg, id) f
                    j <- [|Just|]
                    return $ j `AppE` (DoE $ f' [])
    ch <- [|condH|]
    let stmt = NoBindS $ ch `AppE` conds' `AppE` final'
    return (vars, arg, stmts . (:) stmt)
docToStmt v (DocContent c) = foldM contentToStmt v c

contentToStmt :: Vars -> Content -> Q Vars
contentToStmt (a, b, c) (ContentRaw s) = do
    os <- [|outputString|]
    s' <- lift s
    let stmt = NoBindS $ os `AppE` s'
    return (a, b, c . (:) stmt)
contentToStmt (vars, arg, stmts) (ContentVar d) = do
    (vars', d', stmts') <- bindDeref vars arg d
    oh <- [|outputHtml|]
    let stmt = NoBindS $ oh `AppE` d'
    return (vars', arg, stmts . stmts' . (:) stmt)
contentToStmt (vars, arg, stmts) (ContentUrl d) = do
    (vars', d', stmts') <- bindDeref vars arg d
    ou <- [|outputUrl|]
    let stmt = NoBindS $ ou `AppE` d'
    return (vars', arg, stmts . stmts' . (:) stmt)
contentToStmt (vars, arg, stmts) (ContentEmbed d) = do
    (vars', d', stmts') <- bindDeref vars arg d
    oe <- [|outputEmbed|]
    let stmt = NoBindS $ oe `AppE` d'
    return (vars', arg, stmts . stmts' . (:) stmt)

liftConds :: Scope
          -> Exp
          -> [(Deref, [Doc])]
          -> (Exp -> Exp)
          -> Q Exp
liftConds _vars _arg [] front = do
    nil <- [|[]|]
    return $ front nil
liftConds vars arg ((bool, doc):conds) front = do
    let (base, rest) = shortestPath vars arg bool
    bool' <- identsToVal False (Deref rest) base
    (_, _, doc') <- foldM docToStmt (vars, arg, id) doc
    let pair = TupE [bool', DoE $ doc' []]
    cons <- [|(:)|]
    let front' rest' = front (cons `AppE` pair `AppE` rest')
    liftConds vars arg conds front'

-- | Calls 'hamletWithSettings' with 'defaultHamletSettings'.
hamlet :: QuasiQuoter
hamlet = hamletWithSettings defaultHamletSettings

-- | Calls 'hamletWithSettings' using XHTML 1.0 Strict settings.
xhamlet :: QuasiQuoter
xhamlet = hamletWithSettings $ HamletSettings doctype True where
    doctype =
      "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" " ++
      "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"

-- | A quasi-quoter that converts Hamlet syntax into a function of form:
--
-- argument -> Hamlet url m ()
--
-- Please see accompanying documentation for a description of Hamlet syntax.
-- You must ensure that the type of m, url and argument all work properly with
-- the functions referred to in the template. Of course, worst case scenario is
-- the compiler will catch your mistakes.
hamletWithSettings :: HamletSettings -> QuasiQuoter
hamletWithSettings set =
    QuasiQuoter go $ error "Cannot quasi-quote Hamlet to patterns"
  where
    go s = do
      case parseDoc set s of
        Error s' -> error s'
        Ok d -> docsToExp d

-- deref helper funcs
shortestPath :: Scope
             -> Exp -- ^ original argument
             -> Deref -- ^ path sought
             -> (Exp, [(Bool, Ident)]) -- ^ (base, path from base)
shortestPath vars e (Deref is) = findMatch' svars is e
  where
    svars = sortBy (\(x, _) (y, _)
                  -> compare (length y) (length x)) vars
    findMatch' [] is' e' = (e', is')
    findMatch' (x:xs) is' e' =
        case checkMatch x is' of
            Just y -> y
            Nothing -> findMatch' xs is' e'
    checkMatch :: ([Ident], Exp)
               -> [(Bool, Ident)]
               -> Maybe (Exp, [(Bool, Ident)])
    checkMatch (a, e') b
        | a `isPrefixOf` map snd b = Just (e', drop (length a) b)
        | otherwise = Nothing

-- | Converts a chain of idents and initial 'Exp' to a monadic value.
identsToVal :: Bool -- ^ is initial monadic?
            -> Deref
            -> Exp
            -> Q Exp
identsToVal isMonad (Deref []) e =
    if isMonad
        then return e
        else do
            ret <- [|return|]
            return $ ret `AppE` e
identsToVal isInitMonad (Deref ((isMonad, Ident i):is)) e = do
    case (isInitMonad, isMonad) of
        (False, _) -> do
            let e' = VarE (mkName i) `AppE` e
            identsToVal isMonad (Deref is) e'
        (True, True) -> do
            bind <- [|(>>=)|]
            let e' = InfixE (Just e) bind $ Just $ VarE $ mkName i
            identsToVal True (Deref is) e'
        (True, False) -> do
            fm <- [|fmap|]
            let e' = fm `AppE` (VarE $ mkName i) `AppE` e
            identsToVal True (Deref is) e'

-- | Add a new binding for a 'Deref'
bindDeref :: Scope
          -> Exp -- ^ argument
          -> Deref
          -> Q (Scope, Exp, [Stmt] -> [Stmt])
bindDeref vars arg (Deref []) = return (vars, arg, id)
bindDeref vars arg deref@(Deref idents) =
    case lookup (map snd idents) vars of
        Just e -> return (vars, e, id)
        Nothing -> do
            let front = init idents
                (isMonad, Ident final) = last idents
            (vars', base, stmts) <- bindDeref vars arg $ Deref front
            lh <- [|liftHamlet|]
            let rhs = VarE (mkName final) `AppE` base
            var <- newName $ derefToName deref
            let stmt =
                  if isMonad
                      then BindS (VarP var) $ lh `AppE` rhs
                      else LetS [FunD var
                                  [ Clause [] (NormalB rhs) []
                                  ]]
            let vars'' = (map snd idents, VarE var) : vars'
            return (vars'', VarE var, stmts . (:) stmt)

derefToName :: Deref -> String
derefToName (Deref is') = go is' where
    go [] = ""
    go [(_, Ident i)] = i
    go ((_, Ident i):is) = i ++ "__" ++ go is
