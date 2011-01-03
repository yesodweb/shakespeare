{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Text.Cassius
    ( Cassius
    , Css (..)
    , renderCassius
    , cassius
    , Color (..)
    , colorRed
    , colorBlack
    , cassiusFile
    , cassiusFileDebug
    ) where

import Text.ParserCombinators.Parsec hiding (Line)
import Data.List (intercalate)
import Data.Char (isUpper, isDigit)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText, fromLazyText, singleton)
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import Data.Word (Word8)
import Data.Bits
import System.IO.Unsafe (unsafePerformIO)
import Text.Utf8
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intersperse)

data Color = Color Word8 Word8 Word8
    deriving Show
instance ToCss Color where
    toCss (Color r g b) =
        let (r1, r2) = toHex r
            (g1, g2) = toHex g
            (b1, b2) = toHex b
         in TL.pack $ '#' :
            if r1 == r2 && g1 == g2 && b1 == b2
                then [r1, g1, b1]
                else [r1, r2, g1, g2, b1, b2]
      where
        toHex :: Word8 -> (Char, Char)
        toHex x = (toChar $ shiftR x 4, toChar $ x .&. 15)
        toChar :: Word8 -> Char
        toChar c
            | c < 10 = mkChar c 0 '0'
            | otherwise = mkChar c 10 'A'
        mkChar :: Word8 -> Word8 -> Char -> Char
        mkChar a b' c =
            toEnum $ fromIntegral $ a - b' + fromIntegral (fromEnum c)

colorRed :: Color
colorRed = Color 255 0 0

colorBlack :: Color
colorBlack = Color 0 0 0

renderCss :: Css -> TL.Text
renderCss =
    toLazyText . mconcat . map go
  where
    go (Css' x y) = mconcat
        [ x
        , singleton '{'
        , mconcat $ intersperse (singleton ';') $ map go' $ Map.toList y
        , singleton '}'
        ]
    go' (k, v) = mconcat
        [ fromLazyText k
        , singleton ':'
        , v
        ]

renderCassius :: (url -> [(String, String)] -> String) -> Cassius url -> TL.Text
renderCassius r s = renderCss $ s r

type Css = [Css']
data Css' = Css'
    { cssSelectors :: Builder
    , cssAttributes :: Map TL.Text Builder
    }

type Cassius url = (url -> [(String, String)] -> String) -> Css

class ToCss a where
    toCss :: a -> TL.Text
instance ToCss [Char] where toCss = TL.pack
instance ToCss TS.Text where toCss = TL.fromChunks . return
instance ToCss TL.Text where toCss = id

contentPairToContents :: ContentPair -> Contents
contentPairToContents (x, y) = concat [x, ContentRaw ":" : y]

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
    deriving Show
type Contents = [Content]
type ContentPair = (Contents, Contents)
type Block = (Contents, [ContentPair])

parseBlocks :: Parser [Block]
parseBlocks = catMaybes `fmap` many parseBlock

parseEmptyLine :: Parser ()
parseEmptyLine = do
    try $ skipMany $ oneOf " \t"
    parseComment <|> eol

parseComment :: Parser ()
parseComment = do
    skipMany $ oneOf " \t"
    _ <- string "$#"
    _ <- manyTill anyChar $ eol <|> eof
    return ()

parseIndent :: Parser Int
parseIndent =
    sum `fmap` many ((char ' ' >> return 1) <|> (char '\t' >> return 4))

parseBlock :: Parser (Maybe Block)
parseBlock = do
    indent <- parseIndent
    (emptyBlock >> return Nothing)
        <|> (eof >> if indent > 0 then return Nothing else fail "")
        <|> realBlock indent
  where
    emptyBlock = parseEmptyLine
    realBlock indent = do
        name <- many1 $ parseContent True
        eol
        pairs <- fmap catMaybes $ many $ parsePair' indent
        case pairs of
            [] -> return Nothing
            _ -> return $ Just (name, pairs)
    parsePair' indent = try (parseEmptyLine >> return Nothing)
                    <|> try (Just `fmap` parsePair indent)

parsePair :: Int -> Parser (Contents, Contents)
parsePair minIndent = do
    indent <- parseIndent
    if indent <= minIndent then fail "not indented" else return ()
    key <- manyTill (parseContent False) $ char ':'
    spaces
    value <- manyTill (parseContent True) $ eol <|> eof
    return (key, value)

eol :: Parser ()
eol = (char '\n' >> return ()) <|> (string "\r\n" >> return ())

parseContent :: Bool -> Parser Content
parseContent allowColon = do
    (char '$' >> (parseComment <|> parseDollar <|> parseVar)) <|>
      (char '@' >> (parseAt <|> parseUrl)) <|> safeColon <|> do
        s <- many1 $ noneOf $ (if allowColon then id else (:) ':') "\r\n$@"
        return $ ContentRaw s
  where
    safeColon = try $ do
        _ <- char ':'
        notFollowedBy $ oneOf " \t"
        return $ ContentRaw ":"
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
    parseComment = char '#' >> skipMany (noneOf "\r\n")
                            >> return (ContentRaw "")

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

blocksToCassius :: [(Contents, [ContentPair])] -> Q Exp
blocksToCassius a = do
    r <- newName "_render"
    lamE [varP r] $ listE $ map (blockToCss r) a

cassius :: QuasiQuoter
cassius = QuasiQuoter { quoteExp = cassiusFromString }

cassiusFromString :: String -> Q Exp
cassiusFromString s =
    blocksToCassius
  $ either (error . show) id $ parse parseBlocks s s


blockToCss :: Name -> (Contents, [ContentPair]) -> Q Exp
blockToCss r (sel, props) = do
    css' <- [|Css'|]
    let sel' = contentsToBuilder r sel
    props' <- [|Map.fromList|] `appE` listE (map go props)
    return css' `appE` sel' `appE` return props'
  where
    go (x, y) = tupE [tlt $ contentsToBuilder r x, contentsToBuilder r y]
    tlt = appE [|toLazyText|]

contentsToBuilder :: Name -> [Content] -> Q Exp
contentsToBuilder r contents =
    appE [|mconcat|] $ listE $ map (contentToBuilder r) contents

contentToBuilder :: Name -> Content -> Q Exp
contentToBuilder _ (ContentRaw x) =
    [|fromText . TS.pack|] `appE` litE (StringL x)
contentToBuilder _ (ContentVar d) =
    [|fromLazyText . toCss|] `appE` return (derefToExp d)
contentToBuilder r (ContentUrl u) =
    [|fromText . TS.pack|] `appE`
        (varE r `appE` return (derefToExp u) `appE` listE [])
contentToBuilder r (ContentUrlParam u) =
    [|fromText . TS.pack|] `appE`
        ([|uncurry|] `appE` varE r `appE` return (derefToExp u))

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

cassiusFile :: FilePath -> Q Exp
cassiusFile fp = do
    contents <- fmap bsToChars $ qRunIO $ S8.readFile fp
    cassiusFromString contents

data VarType = VTPlain | VTUrl | VTUrlParam

getVars :: Content -> [(Deref, VarType)]
getVars ContentRaw{} = []
getVars (ContentVar d) = [(d, VTPlain)]
getVars (ContentUrl d) = [(d, VTUrl)]
getVars (ContentUrlParam d) = [(d, VTUrlParam)]

data CDData url = CDPlain TL.Text
                | CDUrl url
                | CDUrlParam (url, [(String, String)])

vtToExp :: (Deref, VarType) -> Q Exp
vtToExp (d, vt) = do
    d' <- lift d
    c' <- c vt
    return $ TupE [d', c' `AppE` derefToExp d]
  where
    c VTPlain = [|CDPlain . toCss|]
    c VTUrl = [|CDUrl|]
    c VTUrlParam = [|CDUrlParam|]

cassiusFileDebug :: FilePath -> Q Exp
cassiusFileDebug fp = do
    s <- fmap bsToChars $ qRunIO $ S8.readFile fp
    let a = either (error . show) id $ parse parseBlocks s s
    c <- mapM vtToExp $ concatMap getVars $ concatMap go a
    cr <- [|cassiusRuntime|]
    return $ cr `AppE` (LitE $ StringL fp) `AppE` ListE c
  where
    go (x, y) = x ++ concatMap go' y
    go' (k, v) = k ++ v

cassiusRuntime :: FilePath -> [(Deref, CDData url)] -> Cassius url
cassiusRuntime fp cd render' = unsafePerformIO $ do
    s <- fmap bsToChars $ qRunIO $ S8.readFile fp
    let a = either (error . show) id $ parse parseBlocks s s
    return $ map go a
  where
    go :: (Contents, [ContentPair]) -> Css'
    go (x, y) = Css' (mconcat $ map go' x) $ Map.fromList $ map go'' y
    go' :: Content -> Builder
    go' (ContentRaw s) = fromText $ TS.pack s
    go' (ContentVar d) =
        case lookup d cd of
            Just (CDPlain s) -> fromLazyText s
            _ -> error $ show d ++ ": expected CDPlain"
    go' (ContentUrl d) =
        case lookup d cd of
            Just (CDUrl u) -> fromText $ TS.pack $ render' u []
            _ -> error $ show d ++ ": expected CDUrl"
    go' (ContentUrlParam d) =
        case lookup d cd of
            Just (CDUrlParam (u, p)) ->
                fromText $ TS.pack $ render' u p
            _ -> error $ show d ++ ": expected CDUrlParam"
    go'' (k, v) = (toLazyText $ mconcat $ map go' k, mconcat $ map go' v)
