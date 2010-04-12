{-# LANGUAGE TemplateHaskell #-}
module Text.Hamlet.Quasi
    where

import Text.Hamlet.Parse
import Text.Hamlet.Monad
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Control.Monad

type Vars = ([(Deref, Exp)], Exp, [Stmt] -> [Stmt])

docsToExp :: [Doc] -> Q Exp
docsToExp docs = do
    arg <- newName "arg"
    (_, _, stmts) <- foldM docToStmt ([], VarE arg, id) docs
    return $ LamE [VarP arg] $ DoE $ stmts []

docToStmt :: Vars -> Doc -> Q Vars
docToStmt (vars, arg, stmts) (DocForall deref ident@(Ident name) inside) = do
    (vars', deref', stmts') <- liftDeref vars arg deref
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
    (vars', d', stmts') <- liftDeref vars arg d
    oh <- [|outputHtml|]
    let stmt = NoBindS $ oh `AppE` d'
    return (vars', arg, stmts . stmts' . (:) stmt)
contentToStmt (vars, arg, stmts) (ContentUrl d) = do
    (vars', d', stmts') <- liftDeref vars arg d
    ou <- [|outputUrl|]
    let stmt = NoBindS $ ou `AppE` d'
    return (vars', arg, stmts . stmts' . (:) stmt)
contentToStmt (vars, arg, stmts) (ContentEmbed d) = do
    d' <- derefToExp vars arg d
    let stmt = BindS (TupP []) d'
    return (vars, arg, stmts . (:) stmt)

derefToExp :: [(Deref, Exp)] -> Exp -> Deref -> Q Exp
derefToExp vars arg d@(Deref is) =
    case lookup d vars of
        Just d' -> return d'
        Nothing -> do
            bind <- [|(>>=)|]
            ret <- [|return|]
            let arg' = ret `AppE` arg
            return $ go arg' bind is
  where
    go rhs _ [] = rhs
    go rhs bind (Ident i:is') =
        let rhs' = bind `AppE` rhs `AppE` VarE (mkName i)
         in go rhs' bind is'

liftDeref :: [(Deref, Exp)] -> Exp -> Deref -> Q ([(Deref, Exp)], Exp, [Stmt] -> [Stmt])
liftDeref vars arg d@(Deref is) =
    case lookup d vars of
        Just d' -> return (vars, d', id)
        Nothing -> do
            var <- newName "var"
            bind <- [|(>>=)|]
            ret <- [|return|]
            let arg' = ret `AppE` arg
            let stmt = go var arg' bind is
            let var' = VarE var
            return ((d, var') : vars, var', (:) stmt)
  where
    go var rhs _ [] = BindS (VarP var) rhs
    go var rhs bind (Ident i:is') =
        let rhs' = bind `AppE` rhs `AppE` VarE (mkName i)
         in go var rhs' bind is'

liftConds :: [(Deref, Exp)] -> Exp -> [(Deref, [Doc])]
          -> (Exp -> Exp)
          -> Q Exp
liftConds _vars _arg [] front = do
    nil <- [|[]|]
    return $ front nil
liftConds vars arg ((bool, doc):conds) front = do
    bool' <- derefToExp vars arg bool
    (_, _, doc') <- foldM docToStmt (vars, arg, id) doc
    let pair = TupE [bool', DoE $ doc' []]
    cons <- [|(:)|]
    let front' rest = front (cons `AppE` pair `AppE` rest)
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
