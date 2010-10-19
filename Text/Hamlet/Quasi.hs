{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
module Text.Hamlet.Quasi
    ( hamlet
    , xhamlet
    , hamlet'
    , xhamlet'
    , hamletDebug
    , hamletWithSettings
    , hamletWithSettings'
    , hamletFile
    , xhamletFile
    , hamletFileWithSettings
    , ToHtml (..)
    , HamletValue (..)
    , varName
    , Html (..)
    , Hamlet
    ) where

import Text.Hamlet.Parse
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Char (isUpper, isDigit)
import qualified Data.ByteString.Char8 as S8
import Data.Monoid (Monoid (..))
import Text.Blaze.Builder.Core (Builder, fromByteString, toLazyByteString)
import Text.Blaze.Builder.Html (fromHtmlEscapedString)
import Data.Maybe (fromMaybe)
import Data.String
import Text.Utf8

instance IsString Html where
    fromString = Html . fromHtmlEscapedString

class ToHtml a where
    toHtml :: a -> Html
instance ToHtml String where
    toHtml = Html . fromHtmlEscapedString
instance ToHtml Html where
    toHtml = id

type Scope = [(Ident, Exp)]

docsToExp :: Scope -> [Doc] -> Q Exp
docsToExp scope docs = do
    exps <- mapM (docToExp scope) docs
    case exps of
        [] -> [|return ()|]
        [x] -> return x
        _ -> return $ DoE $ map NoBindS exps

docToExp :: Scope -> Doc -> Q Exp
docToExp scope (DocForall list ident@(Ident name) inside) = do
    let list' = deref scope list
    name' <- newName name
    let scope' = (ident, VarE name') : scope
    mh <- [|mapM_|]
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
    os <- [|htmlToHamletMonad . Html . fromByteString . S8.pack|]
    let s' = LitE $ StringL $ charsToOctets s
    return $ os `AppE` s'
contentToExp scope (ContentVar d) = do
    str <- [|htmlToHamletMonad . toHtml|]
    return $ str `AppE` deref scope d
contentToExp scope (ContentUrl hasParams d) = do
    ou <- if hasParams
            then [|\(u, p) -> urlToHamletMonad u p|]
            else [|\u -> urlToHamletMonad u []|]
    let d' = deref scope d
    return $ ou `AppE` d'
contentToExp scope (ContentEmbed d) = do
    let d' = deref scope d
    fhv <- [|fromHamletValue|]
    return $ fhv `AppE` d'

-- | Calls 'hamletWithSettings'' with 'defaultHamletSettings'.
hamlet' :: QuasiQuoter
hamlet' = hamletWithSettings' defaultHamletSettings
{-# DEPRECATED hamlet' "Use hamlet directly now" #-}

-- | Calls 'hamletWithSettings'' using XHTML 1.0 Strict settings.
xhamlet' :: QuasiQuoter
xhamlet' = hamletWithSettings' xhtmlHamletSettings
{-# DEPRECATED xhamlet' "Use xhamlet directly now" #-}

-- | Calls 'hamletWithSettings' with 'defaultHamletSettings'.
hamlet :: QuasiQuoter
hamlet = hamletWithSettings defaultHamletSettings

-- | Calls 'hamletWithSettings' with 'debugHamletSettings'.
hamletDebug :: QuasiQuoter
hamletDebug = hamletWithSettings debugHamletSettings

-- | Calls 'hamletWithSettings' using XHTML 1.0 Strict settings.
xhamlet :: QuasiQuoter
xhamlet = hamletWithSettings xhtmlHamletSettings

-- | A quasi-quoter that converts Hamlet syntax into a function of form:
--
-- > (url -> String) -> Html
--
-- Please see accompanying documentation for a description of Hamlet syntax.
hamletWithSettings :: HamletSettings -> QuasiQuoter
hamletWithSettings set =
    QuasiQuoter
        { quoteExp = hamletFromString set
        }

-- | A quasi-quoter that converts Hamlet syntax into a 'Html' ().
--
-- Please see accompanying documentation for a description of Hamlet syntax.
hamletWithSettings' :: HamletSettings -> QuasiQuoter
hamletWithSettings' set =
    QuasiQuoter
        { quoteExp = \s -> do
            x <- hamletFromString set s
            id' <- [|(\y _ -> y) :: String -> [(String, String)] -> String|]
            return $ x `AppE` id'
        }

hamletFromString :: HamletSettings -> String -> Q Exp
hamletFromString set s = do
    case parseDoc set s of
        Error s' -> error s'
        Ok d -> do
            thv <- [|toHamletValue|]
            exp' <- docsToExp [] d
            return $ thv `AppE` exp'

hamletFileWithSettings :: HamletSettings -> FilePath -> Q Exp
hamletFileWithSettings set fp = do
    contents <- fmap bsToChars $ qRunIO $ S8.readFile fp
    hamletFromString set contents

-- | Calls 'hamletFileWithSettings' with 'defaultHamletSettings'.
hamletFile :: FilePath -> Q Exp
hamletFile = hamletFileWithSettings defaultHamletSettings

-- | Calls 'hamletFileWithSettings' using XHTML 1.0 Strict settings.
xhamletFile :: FilePath -> Q Exp
xhamletFile = hamletFileWithSettings xhtmlHamletSettings

deref :: Scope -> Deref -> Exp
deref scope (DerefBranch x y) =
    let x' = deref scope x
        y' = deref scope y
     in x' `AppE` y'
deref scope (DerefLeaf d@(Ident dName)) =
    case lookup d scope of
        Nothing -> varName scope dName
        Just exp' -> exp'

varName :: Scope -> String -> Exp
varName _ "" = error "Illegal empty varName"
varName scope v@(_:_) = fromMaybe (strToExp v) $ lookup (Ident v) scope

strToExp :: String -> Exp
strToExp s@(c:_)
    | all isDigit s = LitE $ IntegerL $ read s
    | isUpper c = ConE $ mkName s
    | otherwise = VarE $ mkName s
strToExp "" = error "strToExp on empty string"

-- | Checks for truth in the left value in each pair in the first argument. If
-- a true exists, then the corresponding right action is performed. Only the
-- first is performed. In there are no true values, then the second argument is
-- performed, if supplied.
condH :: Monad m => [(Bool, m ())] -> Maybe (m ()) -> m ()
condH [] Nothing = return ()
condH [] (Just x) = x
condH ((True, y):_) _ = y
condH ((False, _):rest) z = condH rest z

-- | Runs the second argument with the value in the first, if available.
-- Otherwise, runs the third argument, if available.
maybeH :: Monad m => Maybe v -> (v -> m ()) -> Maybe (m ()) -> m ()
maybeH Nothing _ Nothing = return ()
maybeH Nothing _ (Just x) = x
maybeH (Just v) f _ = f v

newtype Html = Html Builder
    deriving Monoid
instance Show Html where
    show (Html b) = show $ lbsToChars $ toLazyByteString b
instance Eq Html where
    (Html a) == (Html b) = toLazyByteString a == toLazyByteString b

-- | An function generating an 'Html' given a URL-rendering function.
type Hamlet url = (url -> [(String, String)] -> String) -> Html

class Monad (HamletMonad a) => HamletValue a where
    data HamletMonad a :: * -> *
    type HamletUrl a
    toHamletValue :: HamletMonad a () -> a
    htmlToHamletMonad :: Html -> HamletMonad a ()
    urlToHamletMonad :: HamletUrl a -> [(String, String)] -> HamletMonad a ()
    fromHamletValue :: a -> HamletMonad a ()

type Render url = url -> [(String, String)] -> String
instance HamletValue (Hamlet url) where
    newtype HamletMonad (Hamlet url) a =
        HMonad { runHMonad :: Render url -> (Html, a) }
    type HamletUrl (Hamlet url) = url
    toHamletValue = fmap fst . runHMonad
    htmlToHamletMonad x = HMonad $ const (x, ())
    urlToHamletMonad url pairs = HMonad $ \r ->
        (Html $ fromHtmlEscapedString $ r url pairs, ())
    fromHamletValue f = HMonad $ \r -> (f r, ())
instance Monad (HamletMonad (Hamlet url)) where
    return x = HMonad $ const (mempty, x)
    (HMonad f) >>= g = HMonad $ \render ->
        let (html1, x) = f render
            (html2, y) = runHMonad (g x) render
         in (html1 `mappend` html2, y)
data NoConstructor
instance HamletValue Html where
    newtype HamletMonad Html a = HtmlMonad { runHtmlMonad :: (Html, a) }
    type HamletUrl Html = NoConstructor
    toHamletValue = fst . runHtmlMonad
    htmlToHamletMonad x = HtmlMonad (x, ())
    urlToHamletMonad = error "urlToHamletMonad on NoConstructor"
    fromHamletValue h = HtmlMonad (h, ())
instance Monad (HamletMonad Html) where
    return x = HtmlMonad (mempty, x)
    HtmlMonad (html1, x) >>= g = HtmlMonad $
        let HtmlMonad (html2, y) = g x
         in (html1 `mappend` html2, y)
