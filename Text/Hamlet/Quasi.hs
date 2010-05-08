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
import Data.Char (isUpper)

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
docToStmt (vars, arg, stmts) (DocMaybe deref ident@(Ident name) inside) = do
    (vars', base, stmts') <- bindDeref vars arg deref
    ident' <- newName name
    let vars'' = ([ident], VarE ident') : vars'
    (_, _, inside') <- foldM docToStmt (vars'', arg, id) inside
    let dos = LamE [VarP ident'] $ DoE $ inside' []
    mh <- [|maybeH|]
    let stmt = NoBindS $ mh `AppE` base `AppE` dos
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
shortestPath scope arg (Deref is) = findMatch' svars
  where
    svars = sortBy (\(x, _) (y, _)
                  -> compare (length y) (length x)) scope
    findMatch' [] =
        case is of
            ((False, top):rest) -> (getBase scope arg top, rest)
            ((True, _):_) -> error "shortestPath: first segment is monadic"
            [] -> error "shortestPath called with null deref"
    findMatch' (x:xs) =
        case checkMatch x of
            Just y -> y
            Nothing -> findMatch' xs
    checkMatch :: ([Ident], Exp)
               -> Maybe (Exp, [(Bool, Ident)])
    checkMatch (a, e')
        | a `isPrefixOf` map snd is = Just (e', drop (length a) is)
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

getBase :: Scope
        -> Exp -- ^ argument
        -> Ident
        -> Exp
getBase _ arg (Ident "") = arg
getBase _ _ (Ident name@(x:_))
    | isUpper x = ConE $ mkName name
getBase scope _ (Ident top) =
    case lookup [Ident top] scope of
        Nothing -> VarE $ mkName top
        Just e -> e

bindMonadicVar :: String -> Exp -> Q (Exp, [Stmt] -> [Stmt])
bindMonadicVar name e = do
    lh <- [|liftHamlet|]
    name' <- newName name
    let stmt = BindS (VarP name') $ lh `AppE` e
    return (VarE name', (:) stmt)

-- | Add a new binding for a 'Deref'
bindDeref :: Scope
          -> Exp -- ^ argument
          -> Deref
          -> Q (Scope, Exp, [Stmt] -> [Stmt])
bindDeref _ _ (Deref []) = error "bindDeref with null deref"
bindDeref scope arg (Deref ((isMonad, base):rest)) = do
    let base' = getBase scope arg base
    (base'', stmts) <-
        if isMonad
            then bindMonadicVar "_FIXMEvarName" base'
            else return (base', id)
    (x, y, z, _) <- foldM go (scope, base'', stmts, [base]) rest
    return (x, y, z)
  where
    go :: (Scope, Exp, [Stmt] -> [Stmt], [Ident])
       -> (Bool, Ident)
       -> Q (Scope, Exp, [Stmt] -> [Stmt], [Ident])
    go (scope, base, stmts, front) (isMonad, Ident func) = do
        let rhs = VarE (mkName func) `AppE` base
        name <- newName func
        stmt <-
            if isMonad
                then do
                    lh <- [|liftHamlet|]
                    return $ BindS (VarP name) $ lh `AppE` rhs
                else return $ LetS [FunD name
                                    [ Clause [] (NormalB rhs) []
                                    ]]
        let front' = front ++ [Ident func]
        return ((front', VarE name) : scope, VarE name,
                stmts . (:) stmt, front')

derefToName :: Deref -> String
derefToName (Deref is') = go is' where
    go [] = ""
    go [(_, Ident i)] = i
    go ((_, Ident i):is) = i ++ "__" ++ go is
