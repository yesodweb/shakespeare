{-# LANGUAGE TemplateHaskell #-}
module Text.Hamlet.Quasi
    where

import Text.Hamlet.Parse
import Text.Hamlet.Monad
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

docsToExp :: [Doc] -> Q Exp
docsToExp = fmap (DoE . concat) . mapM docToStmt

docToStmt :: Doc -> Q [Stmt]
docToStmt (DocForall _ _ _) = return [] -- FIXME
docToStmt (DocCond _ _) = return [] -- FIXME
docToStmt (DocContent c) = mapM contentToStmt c

contentToStmt :: Content -> Q Stmt
contentToStmt (ContentRaw s) = do
    os <- [|outputString|]
    s' <- lift s
    return $ NoBindS $ os `AppE` s'
contentToStmt (ContentVar d) = do
    d' <- liftDeref d
    oh <- [|outputHtml|]
    bind <- [|(>>=)|]
    return $ NoBindS $ bind `AppE` d' `AppE` oh
contentToStmt (ContentUrl d) = do
    d' <- liftDeref d
    ou <- [|outputUrl|]
    bind <- [|(>>=)|]
    return $ NoBindS $ bind `AppE` d' `AppE` ou

liftDeref :: Deref -> Q Exp
liftDeref (Deref is) = do
    bind <- [|(>>=)|]
    ret <- [|return|]
    let arg = ret `AppE` VarE (mkName "arg")
    return $ go arg bind is
  where
    go arg _ [] = arg
    go arg bind (Ident i:is) = go (bind `AppE` arg `AppE` VarE (mkName i)) bind is

hamlet :: QuasiQuoter
hamlet = QuasiQuoter go $ error "Cannot quasi-quote Hamlet to patterns"
  where
    go s = do
      case parseDoc s of
        Error s -> error s
        Ok d -> docsToExp d
