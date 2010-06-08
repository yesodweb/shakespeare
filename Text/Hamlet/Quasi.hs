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
import Data.Monoid (mconcat, mappend, mempty)
import Text.Blaze (unsafeBytestring)

type Scope = [(Ident, Exp)]

docsToExp :: Exp -> Scope -> [Doc] -> Q Exp
docsToExp render scope docs = do
    exps <- mapM (docToExp render scope) docs
    ma <- [|mappend|]
    me <- [|mempty|]
    return $ if null exps then me else foldr1 (go ma) exps
  where
    go ma x y = ma `AppE` x `AppE` y

docToExp :: Exp -> Scope -> Doc -> Q Exp
docToExp render scope (DocForall list ident@(Ident name) inside) = do
    let list' = deref scope list
    name' <- newName name
    let scope' = (ident, VarE name') : scope
    mh <- [|\a -> mconcat . map a|]
    inside' <- docsToExp render scope' inside
    let lam = LamE [VarP name'] inside'
    return $ mh `AppE` lam `AppE` list'
docToExp render scope (DocMaybe val ident@(Ident name) inside mno) = do
    let val' = deref scope val
    name' <- newName name
    let scope' = (ident, VarE name') : scope
    inside' <- docsToExp render scope' inside
    let inside'' = LamE [VarP name'] inside'
    ninside' <- case mno of
                    Nothing -> [|Nothing|]
                    Just no -> do
                        no' <- docsToExp render scope no
                        j <- [|Just|]
                        return $ j `AppE` no'
    mh <- [|maybeH|]
    return $ mh `AppE` val' `AppE` inside'' `AppE` ninside'
docToExp render scope (DocCond conds final) = do
    conds' <- mapM go conds
    final' <- case final of
                Nothing -> [|Nothing|]
                Just f -> do
                    f' <- docsToExp render scope f
                    j <- [|Just|]
                    return $ j `AppE` f'
    ch <- [|condH|]
    return $ ch `AppE` ListE conds' `AppE` final'
  where
    go :: (Deref, [Doc]) -> Q Exp
    go (d, docs) = do
        let d' = deref scope d
        docs' <- docsToExp render scope docs
        return $ TupE [d', docs']
docToExp render v (DocContent c) = contentToExp render v c

contentToExp :: Exp -> Scope -> Content -> Q Exp
contentToExp _ _ (ContentRaw s) = do
    os <- [|unsafeBytestring . S8.pack|]
    let s' = LitE $ StringL $ S8.unpack $ BSU.fromString s
    return $ os `AppE` s'
contentToExp _ scope (ContentVar d) = return $ deref scope d
contentToExp render scope (ContentUrl hasParams d) = do
    ou <- if hasParams then [|outputUrlParams|] else [|outputUrl|]
    let d' = deref scope d
    return $ ou `AppE` render `AppE` d'
contentToExp render scope (ContentEmbed d) = do
    let d' = deref scope d
    return (d' `AppE` render)

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
        Ok d -> do
            render <- newName "render"
            func <- docsToExp (VarE render) [] d
            return $ LamE [VarP render] func

deref :: Scope -> Deref -> Exp
deref _ (Deref []) = error "Invalid empty deref"
deref scope (Deref (z@(Ident zName):y)) =
    let z' = case lookup z scope of
                Nothing -> varName zName
                Just zExp -> zExp
     in foldr go z' $ reverse y
  where
    varName "" = error "Illegal empty varName"
    varName v@(s:_) =
        case lookup (Ident v) scope of
            Just e -> e
            Nothing ->
                if isUpper s
                    then ConE $ mkName v
                    else VarE $ mkName v
    go (Ident func) z' = varName func `AppE` z'
