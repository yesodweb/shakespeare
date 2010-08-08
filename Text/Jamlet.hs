{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Text.Jamlet
    ( Jamlet
    , renderJamlet
    , jamlet
    , jamletFile
    , jamletFileDebug
    ) where

import Text.ParserCombinators.Parsec hiding (Line)
import Data.Neither (AEither (..), either)
import Data.Traversable (sequenceA)
import Control.Applicative ((<$>))
import Data.List (intercalate)
import Data.Char (isUpper, isDigit)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Text.Blaze.Builder.Core (Builder, fromByteString, toLazyByteString)
import Text.Blaze.Builder.Utf8 (fromString)
import Data.Maybe (catMaybes)
import Prelude hiding (either)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import Data.Word (Word8)
import Data.Bits
import Text.Hamlet.Quasi (showParams)
import System.IO.Unsafe (unsafePerformIO)

renderJavascript :: Javascript -> L.ByteString
renderJavascript (Javascript b) = toLazyByteString b

renderJamlet :: (url -> String) -> Jamlet url -> L.ByteString
renderJamlet r s = renderJavascript $ s r

newtype Javascript = Javascript Builder
    deriving Monoid
type Jamlet url = (url -> String) -> Javascript

class ToJavascript a where
    toJavascript :: a -> String
instance ToJavascript [Char] where toJavascript = id

data Deref = DerefLeaf String
           | DerefBranch Deref Deref
    deriving (Show, Eq)

instance Lift Deref where
    lift (DerefLeaf s) = do
        dl <- [|DerefLeaf|]
        return $ dl `AppE` (LitE $ StringL s)
    lift (DerefBranch x y) = do
        x' <- lift x
        y' <- lift y
        db <- [|DerefBranch|]
        return $ db `AppE` x' `AppE` y'

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
    (char '$' >> (parseDollar <|> parseVar)) <|>
      (char '@' >> (parseAt <|> parseUrl)) <|> do
      (char '^' >> (parseCaret <|> parseMix)) <|> do
        s <- many1 $ noneOf "$@^"
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
    parseDollar = char '$' >> return (ContentRaw "$")
    parseVar = do
        d <- parseDeref
        _ <- char '$'
        return $ ContentVar d

parseDeref :: Parser Deref
parseDeref =
    deref
  where
    derefParens = between (char '(') (char ')') deref
    derefSingle = derefParens <|> fmap DerefLeaf ident
    deref = do
        let delim = (char '.' <|> (many1 (char ' ') >> return ' '))
        x <- derefSingle
        xs <- many $ delim >> derefSingle
        return $ foldr1 DerefBranch $ x : xs
    ident = many1 (alphaNum <|> char '_' <|> char '\'')

compressContents :: Contents -> Contents
compressContents [] = []
compressContents (ContentRaw x:ContentRaw y:z) =
    compressContents $ ContentRaw (x ++ y) : z
compressContents (x:y) = x : compressContents y

contentsToJamlet :: [Content] -> Q Exp
contentsToJamlet a = do
    r <- newName "_render"
    c <- mapM (contentToJavascript r) $ compressContents a
    d <- case c of
            [] -> [|mempty|]
            [x] -> return x
            _ -> do
                mc <- [|mconcat|]
                return $ mc `AppE` ListE c
    return $ LamE [VarP r] d

jamlet :: QuasiQuoter
jamlet =
    QuasiQuoter jamletFromString p
  where
    p = error "jamlet quasi-quoter for patterns does not exist"

jamletFromString :: String -> Q Exp
jamletFromString s = do
    let a = either (error . show) id $ parse parseContents s s
    contentsToJamlet a

contentToJavascript :: Name -> Content -> Q Exp
contentToJavascript _ (ContentRaw s') = do
    let d = S8.unpack $ BSU.fromString s'
    ts <- [|Javascript . fromByteString . S8.pack|]
    return $ ts `AppE` LitE (StringL d)
contentToJavascript _ (ContentVar d) = do
    ts <- [|Javascript . fromString . toJavascript|]
    return $ ts `AppE` derefToExp d
contentToJavascript r (ContentUrl d) = do
    ts <- [|Javascript . fromString|]
    return $ ts `AppE` (VarE r `AppE` derefToExp d)
contentToJavascript r (ContentUrlParam d) = do
    ts <- [|Javascript . fromString|]
    up <- [|\r' (u, p) -> r' u ++ showParams p|]
    return $ ts `AppE` (up `AppE` VarE r `AppE` derefToExp d)
contentToJavascript r (ContentMix d) = do
    return $ derefToExp d `AppE` VarE r

derefToExp :: Deref -> Exp
derefToExp (DerefBranch x y) =
    let x' = derefToExp x
        y' = derefToExp y
     in x' `AppE` y'
derefToExp (DerefLeaf "") = error "Illegal empty ident"
derefToExp (DerefLeaf v@(s:_))
    | all isDigit v = LitE $ IntegerL $ read v
    | isUpper s = ConE $ mkName v
    | otherwise = VarE $ mkName v

jamletFile :: FilePath -> Q Exp
jamletFile fp = do
    contents <- fmap BSU.toString $ qRunIO $ S8.readFile fp
    jamletFromString contents

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
                | JDMixin (Jamlet url)

vtToExp :: (Deref, VarType) -> Q Exp
vtToExp (d, vt) = do
    d' <- lift d
    c' <- c vt
    return $ TupE [d', c' `AppE` derefToExp d]
  where
    c VTPlain = [|JDPlain . toJavascript|]
    c VTUrl = [|JDUrl|]
    c VTUrlParam = [|JDUrlParam|]
    c VTMixin = [|JDMixin|]

jamletFileDebug :: FilePath -> Q Exp
jamletFileDebug fp = do
    s <- fmap BSU.toString $ qRunIO $ S8.readFile fp
    let a = either (error . show) id $ parse parseContents s s
        b = concatMap getVars a
    c <- mapM vtToExp b
    cr <- [|jamletRuntime|]
    return $ cr `AppE` (LitE $ StringL fp) `AppE` ListE c

jamletRuntime :: FilePath -> [(Deref, JDData url)] -> Jamlet url
jamletRuntime fp cd render' = unsafePerformIO $ do
    s <- fmap BSU.toString $ qRunIO $ S8.readFile fp
    let a = either (error . show) id $ parse parseContents s s
    return $ mconcat $ map go a
  where
    go :: Content -> Javascript
    go (ContentRaw s) = Javascript $ fromString s
    go (ContentVar d) =
        case lookup d cd of
            Just (JDPlain s) -> Javascript $ fromString s
            _ -> error $ show d ++ ": expected JDPlain"
    go (ContentUrl d) =
        case lookup d cd of
            Just (JDUrl u) -> Javascript $ fromString $ render' u
            _ -> error $ show d ++ ": expected JDUrl"
    go (ContentUrlParam d) =
        case lookup d cd of
            Just (JDUrlParam (u, p)) ->
                Javascript $ fromString $ render' u ++ showParams p
            _ -> error $ show d ++ ": expected JDUrlParam"
    go (ContentMix d) =
        case lookup d cd of
            Just (JDMixin m) -> m render'
            _ -> error $ show d ++ ": expected JDMixin"
