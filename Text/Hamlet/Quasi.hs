{-# LANGUAGE TemplateHaskell #-}
module Text.Hamlet.Quasi
    where

import Text.Hamlet.Parse
import Text.Hamlet.Monad
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Ppr
import Language.Haskell.TH.PprLib hiding (Doc)
import Control.Monad

type Vars = ([(Deref, Exp)], Exp, [Stmt] -> [Stmt])

printStmts s = do
    print $ to_HPJ_Doc $ ppr $ DoE s

docsToExp :: [Doc] -> Q Exp
docsToExp docs = do
    arg <- newName "arg"
    (_, _, stmts) <- foldM docToStmt ([], VarE arg, id) docs
    --qRunIO $ printStmts $ stmts []
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
docToStmt v (DocCond _ _) = return v -- FIXME
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
    go var rhs bind (Ident i:is) =
        let rhs' = bind `AppE` rhs `AppE` VarE (mkName i)
         in go var rhs' bind is

hamlet :: QuasiQuoter
hamlet = QuasiQuoter go $ error "Cannot quasi-quote Hamlet to patterns"
  where
    go s = do
      case parseDoc s of
        Error s -> error s
        Ok d -> docsToExp d
