{-# LANGUAGE TemplateHaskell #-}
module Text.Hamlet.Quasi
    ( hamlet
    , hamletWithSettings
    ) where

import Text.Hamlet.Parse
import Text.Hamlet.Monad
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Control.Monad
import Data.List (sortBy, isPrefixOf)

type Vars = ([(Deref, Exp)], Exp, [Stmt] -> [Stmt])

docsToExp :: [Doc] -> Q Exp
docsToExp docs = do
    arg <- newName "_arg"
    (_, _, stmts) <- foldM docToStmt ([], VarE arg, id) docs
    return $ LamE [VarP arg] $ DoE $ stmts []

docToStmt :: Vars -> Doc -> Q Vars
docToStmt (vars, arg, stmts) (DocForall deref ident@(Ident name) inside) = do
    (vars', deref', stmts') <- bindDeref vars arg deref
    mh <- [|mapH|]
    ident' <- newName name
    let vars'' = (Deref [ident], VarE ident') : vars'
    (_, _, inside') <- foldM docToStmt (vars'', arg, id) inside
    let dos = LamE [VarP ident'] $ DoE $ inside' []
    let stmt = NoBindS $ mh `AppE` dos `AppE` deref'
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
    d' <- hamletDeref vars arg d
    let stmt = BindS (TupP []) d'
    return (vars, arg, stmts . (:) stmt)

liftConds :: [(Deref, Exp)] -> Exp -> [(Deref, [Doc])]
          -> (Exp -> Exp)
          -> Q Exp
liftConds _vars _arg [] front = do
    nil <- [|[]|]
    return $ front nil
liftConds vars arg ((Deref bool, doc):conds) front = do
    let (base, rest) = shortestPath vars arg bool
    bool' <- identsToVal rest base
    (_, _, doc') <- foldM docToStmt (vars, arg, id) doc
    let pair = TupE [bool', DoE $ doc' []]
    cons <- [|(:)|]
    let front' rest' = front (cons `AppE` pair `AppE` rest')
    liftConds vars arg conds front'

-- | Calls 'hamletWithSettings' with 'defaultHamletSettings'.
hamlet :: QuasiQuoter
hamlet = hamletWithSettings defaultHamletSettings

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
shortestPath :: [(Deref, Exp)] -- ^ scope
             -> Exp -- ^ original argument
             -> [Ident] -- ^ path sought
             -> (Exp, [Ident]) -- ^ (base, path from base)
shortestPath vars e is = findMatch' svars is e
  where
    svars = sortBy (\(Deref x, _) (Deref y, _)
                  -> compare (length y) (length x)) vars
    findMatch' [] is' e' = (e', is')
    findMatch' (x:xs) is' e' =
        case checkMatch x is' of
            Just y -> y
            Nothing -> findMatch' xs is' e'
    checkMatch :: (Deref, Exp) -> [Ident] -> Maybe (Exp, [Ident])
    checkMatch (Deref a, e') b
        | a `isPrefixOf` b = Just (e', drop (length a) b)
        | otherwise = Nothing

-- | Converts a chain of idents and initial 'Exp' to a monadic value.
identsToVal :: [Ident] -> Exp -> Q Exp
identsToVal [] e = do
    ret <- [|return|]
    return $ ret `AppE` e
identsToVal (Ident i:is) e = do
    let e' = VarE (mkName i) `AppE` e
    addBinds is e'

-- | Tacks on a series of monadic binds to a monadic 'Exp'.
addBinds :: [Ident] -> Exp -> Q Exp
addBinds [] e = return e
addBinds (Ident i:is) e = do
    bind <- [|(>>=)|]
    let e' = InfixE (Just e) bind $ Just $ VarE $ mkName i
    addBinds is e'

-- | Add a new binding for a 'Deref'
bindDeref :: [(Deref, Exp)] -- ^ scope
          -> Exp -- ^ argument
          -> Deref
          -> Q ([(Deref, Exp)], Exp, [Stmt] -> [Stmt])
bindDeref vars arg (Deref []) = return (vars, arg, id)
bindDeref vars arg deref@(Deref idents) =
    case lookup deref vars of
        Just e -> return (vars, e, id)
        Nothing -> do
            let front = init idents
                Ident final = last idents
            (vars', base, stmts) <- bindDeref vars arg $ Deref front
            lh <- [|liftHamlet|]
            let rhs = VarE (mkName final) `AppE` base
            var <- newName "var"
            let stmt = BindS (VarP var) $ lh `AppE` rhs
            let vars'' = (deref, VarE var) : vars'
            return (vars'', VarE var, stmts . (:) stmt)

-- | Converts a 'Deref' into a Hamlet value.
hamletDeref :: [(Deref, Exp)] -- ^ scope
            -> Exp -- ^ argument
            -> Deref
            -> Q Exp
hamletDeref vars arg (Deref idents) = do
    let (base, rest) = shortestPath vars arg idents
    case rest of
        [] -> return base
        _ -> do
            let front = init rest
                Ident back = last rest
            front' <- identsToVal front base
            lh <- [|liftHamlet|]
            let front'' = lh `AppE` front'
            bind <- [|(>>=)|]
            return $ InfixE (Just front'') bind $ Just $ VarE $ mkName back
