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
import Data.Char (isUpper)
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Char8 as S8
import Data.Monoid (mconcat)

type Scope = [(Ident, Exp)]

docsToExp :: Scope -> [Doc] -> Q Exp
docsToExp scope docs = do
    exps <- mapM (docToExp scope) docs
    mc <- [|mconcat|]
    return $ mc `AppE` ListE exps

docToExp :: Scope -> Doc -> Q Exp
docToExp scope (DocForall list ident@(Ident name) inside) = do
    let list' = deref scope list
    name' <- newName name
    let scope' = (ident, VarE name') : scope
    mh <- [|mapH|]
    inside' <- docsToExp scope' inside
    let lam = LamE [VarP name'] inside'
    return $ mh `AppE` lam `AppE` list'
docToExp scope (DocMaybe val ident@(Ident name) inside mno) = do
    let val' = deref scope val
    name' <- newName name
    let scope' = (ident, VarE name') : scope
    inside' <- docsToExp scope' inside
    let inside'' = LamE [VarP name'] inside'
    ninside' <- case mno of
                    Nothing -> [|Nothing|]
                    Just no -> do
                        no' <- docsToExp scope no
                        j <- [|Just|]
                        return $ j `AppE` no'
    mh <- [|maybeH|]
    return $ mh `AppE` val' `AppE` inside'' `AppE` ninside'
docToExp scope (DocCond conds final) = do
    conds' <- mapM go conds
    final' <- case final of
                Nothing -> [|Nothing|]
                Just f -> do
                    f' <- docsToExp scope f
                    j <- [|Just|]
                    return $ j `AppE` f'
    ch <- [|condH|]
    return $ ch `AppE` ListE conds' `AppE` final'
  where
    go :: (Deref, [Doc]) -> Q Exp
    go (d, docs) = do
        let d' = deref scope d
        docs' <- docsToExp scope docs
        return $ TupE [d', docs']
docToExp v (DocContent c) = contentToExp v c

contentToExp :: Scope -> Content -> Q Exp
contentToExp _ (ContentRaw s) = do
    os <- [|outputOctets|]
    let s' = LitE $ StringL $ S8.unpack $ BSU.fromString s
    return $ os `AppE` s'
contentToExp scope (ContentVar d) = do
    oh <- [|outputHtml|]
    let d' = deref scope d
    return $ oh `AppE` d'
contentToExp scope (ContentUrl hasParams d) = do
    ou <- if hasParams then [|outputUrlParams|] else [|outputUrl|]
    let d' = deref scope d
    return $ ou `AppE` d'
contentToExp scope (ContentEmbed d) = do
    oe <- [|outputEmbed|]
    let d' = deref scope d
    return $ oe `AppE` d'

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
        Ok d -> docsToExp [] d

deref :: Scope -> Deref -> Exp
deref _ (Deref []) = error "Invalid empty deref"
deref scope (Deref (z@(Ident zName):y)) =
    let z' = case lookup z scope of
                Nothing -> varName zName
                Just zExp -> zExp
     in foldr go z' $ reverse y
  where
    varName "" = error "Illegal empty varName"
    varName v@(s:_)
        | isUpper s = ConE $ mkName v
        | otherwise = VarE $ mkName v
    go (Ident func) z' = varName func `AppE` z'
