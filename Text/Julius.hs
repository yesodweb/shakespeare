{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Text.Julius
    ( Julius
    , Javascript (..)
    , ToJavascript (..)
    , renderJulius
    , julius
    , juliusFile
    , juliusFileDebug
    ) where

import Text.ParserCombinators.Parsec hiding (Line)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText)
import Data.Monoid
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Text.Hamlet.Quasi (readUtf8File)
import Text.Shakespeare

renderJavascript :: Javascript -> TL.Text
renderJavascript (Javascript b) = toLazyText b

renderJulius :: (url -> [(String, String)] -> String) -> Julius url -> TL.Text
renderJulius r s = renderJavascript $ s r

newtype Javascript = Javascript Builder
    deriving Monoid
type Julius url = (url -> [(String, String)] -> String) -> Javascript

class ToJavascript a where -- FIXME use Text instead of String for efficiency? or a builder directly?
    toJavascript :: a -> String
instance ToJavascript [Char] where toJavascript = id
instance ToJavascript TS.Text where toJavascript = TS.unpack
instance ToJavascript TL.Text where toJavascript = TL.unpack

data Content = ContentRaw String
             | ContentVar Deref
             | ContentUrl Deref
             | ContentUrlParam Deref
             | ContentMix Deref
    deriving Show
type Contents = [Content]

parseContents :: Parser Contents
parseContents = many1 parseContent

parseContent :: Parser Content
parseContent = do
    (char '%' >> (parsePercent <|> parseVar)) <|>
      (char '@' >> (parseAt <|> parseUrl)) <|> do
      (char '^' >> (parseCaret <|> parseMix)) <|> do
        s <- many1 $ noneOf "%@^"
        return $ ContentRaw s
  where
    parseCaret = char '^' >> return (ContentRaw "^")
    parseMix = do
        d <- parseDeref
        _ <- char '^'
        return $ ContentMix d
    parseAt = char '@' >> return (ContentRaw "@")
    parseUrl = do
        c <- (char '?' >> return ContentUrlParam) <|> return ContentUrl
        d <- parseDeref
        _ <- char '@'
        return $ c d
    parsePercent = char '%' >> return (ContentRaw "%")
    parseVar = do
        d <- parseDeref
        _ <- char '%'
        return $ ContentVar d

compressContents :: Contents -> Contents
compressContents [] = []
compressContents (ContentRaw x:ContentRaw y:z) =
    compressContents $ ContentRaw (x ++ y) : z
compressContents (x:y) = x : compressContents y

contentsToJulius :: [Content] -> Q Exp
contentsToJulius a = do
    r <- newName "_render"
    c <- mapM (contentToJavascript r) $ compressContents a
    d <- case c of
            [] -> [|mempty|]
            [x] -> return x
            _ -> do
                mc <- [|mconcat|]
                return $ mc `AppE` ListE c
    return $ LamE [VarP r] d

julius :: QuasiQuoter
julius = QuasiQuoter { quoteExp = juliusFromString }

juliusFromString :: String -> Q Exp
juliusFromString s = do
    let a = either (error . show) id $ parse parseContents s s
    contentsToJulius a

contentToJavascript :: Name -> Content -> Q Exp
contentToJavascript _ (ContentRaw s') = do
    ts <- [|Javascript . fromText . TS.pack|]
    return $ ts `AppE` LitE (StringL s')
contentToJavascript _ (ContentVar d) = do
    ts <- [|Javascript . fromText . TS.pack . toJavascript|]
    return $ ts `AppE` derefToExp [] d
contentToJavascript r (ContentUrl d) = do
    ts <- [|Javascript . fromText . TS.pack|]
    return $ ts `AppE` (VarE r `AppE` derefToExp [] d `AppE` ListE [])
contentToJavascript r (ContentUrlParam d) = do
    ts <- [|Javascript . fromText . TS.pack|]
    up <- [|\r' (u, p) -> r' u p|]
    return $ ts `AppE` (up `AppE` VarE r `AppE` derefToExp [] d)
contentToJavascript r (ContentMix d) = do
    return $ derefToExp [] d `AppE` VarE r

juliusFile :: FilePath -> Q Exp
juliusFile fp = do
    contents <- qRunIO $ readUtf8File fp
    juliusFromString $ TL.unpack contents

data VarType = VTPlain | VTUrl | VTUrlParam | VTMixin

getVars :: Content -> [(Deref, VarType)]
getVars ContentRaw{} = []
getVars (ContentVar d) = [(d, VTPlain)]
getVars (ContentUrl d) = [(d, VTUrl)]
getVars (ContentUrlParam d) = [(d, VTUrlParam)]
getVars (ContentMix d) = [(d, VTMixin)]

data JDData url = JDPlain String
                | JDUrl url
                | JDUrlParam (url, [(String, String)])
                | JDMixin (Julius url)

vtToExp :: (Deref, VarType) -> Q Exp
vtToExp (d, vt) = do
    d' <- lift d
    c' <- c vt
    return $ TupE [d', c' `AppE` derefToExp [] d]
  where
    c VTPlain = [|JDPlain . toJavascript|]
    c VTUrl = [|JDUrl|]
    c VTUrlParam = [|JDUrlParam|]
    c VTMixin = [|JDMixin|]

juliusFileDebug :: FilePath -> Q Exp
juliusFileDebug fp = do
    s <- qRunIO $ fmap TL.unpack $ readUtf8File fp
    let a = either (error . show) id $ parse parseContents s s
        b = concatMap getVars a
    c <- mapM vtToExp b
    cr <- [|juliusRuntime|]
    return $ cr `AppE` (LitE $ StringL fp) `AppE` ListE c

juliusRuntime :: FilePath -> [(Deref, JDData url)] -> Julius url
juliusRuntime fp cd render' = unsafePerformIO $ do
    s <- fmap TL.unpack $ readUtf8File fp
    let a = either (error . show) id $ parse parseContents s s
    return $ mconcat $ map go a
  where
    go :: Content -> Javascript
    go (ContentRaw s) = Javascript $ fromText $ TS.pack s
    go (ContentVar d) =
        case lookup d cd of
            Just (JDPlain s) -> Javascript $ fromText $ TS.pack s
            _ -> error $ show d ++ ": expected JDPlain"
    go (ContentUrl d) =
        case lookup d cd of
            Just (JDUrl u) -> Javascript $ fromText $ TS.pack $ render' u []
            _ -> error $ show d ++ ": expected JDUrl"
    go (ContentUrlParam d) =
        case lookup d cd of
            Just (JDUrlParam (u, p)) ->
                Javascript $ fromText $ TS.pack $ render' u p
            _ -> error $ show d ++ ": expected JDUrlParam"
    go (ContentMix d) =
        case lookup d cd of
            Just (JDMixin m) -> m render'
            _ -> error $ show d ++ ": expected JDMixin"
