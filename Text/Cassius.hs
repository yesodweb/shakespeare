{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
module Text.Cassius
    ( -- * Datatypes
      Cassius
    , Css
      -- * Type class
    , ToCss (..)
      -- * Rendering
    , renderCassius
    , renderCss
      -- * Parsing
    , cassius
    , cassiusFile
    , cassiusFileDebug
      -- * ToCss instances
      -- ** Color
    , Color (..)
    , colorRed
    , colorBlack
      -- ** Size
    , mkSize
    , AbsoluteUnit (..)
    , AbsoluteSize (..)
    , absoluteSize
    , EmSize (..)
    , ExSize (..)
    , PercentageSize (..)
    , percentageSize
    , PixelSize (..)
#if HAMLET6TO7
    , parseBlocks
    , Content (..)
    , compressBlock
#endif
    ) where

import Text.MkSizeType
import Text.Shakespeare
import Text.ParserCombinators.Parsec hiding (Line)
import Text.Printf (printf)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText, fromLazyText, singleton)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Word (Word8)
import Data.Bits
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Text.Hamlet.Quasi (readUtf8File)
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
    { _cssSelectors :: Builder
    , _cssAttributes :: Map TL.Text Builder
    }

type Cassius url = (url -> [(String, String)] -> String) -> Css

class ToCss a where
    toCss :: a -> TL.Text
instance ToCss [Char] where toCss = TL.pack
instance ToCss TS.Text where toCss = TL.fromChunks . return
instance ToCss TL.Text where toCss = id

data Content = ContentRaw String
             | ContentVar Deref
             | ContentUrl Deref
             | ContentUrlParam Deref
    deriving (Show, Eq)
type Contents = [Content]
type ContentPair = (Contents, Contents)
type Block = (Contents, [ContentPair])

parseBlocks :: Parser [Block]
parseBlocks = (map compressBlock . catMaybes) `fmap` many parseBlock

compressBlock :: Block -> Block
compressBlock (x, y) =
    (cc x, map go y)
  where
    go (k, v) = (cc k, cc v)
    cc [] = []
    cc (ContentRaw a:ContentRaw b:c) = cc $ ContentRaw (a ++ b) : c
    cc (a:b) = a : cc b

parseEmptyLine :: Parser ()
parseEmptyLine = do
    try $ skipMany $ oneOf " \t"
    parseComment <|> eol

parseComment :: Parser ()
parseComment = do
    _ <- try (skipMany (oneOf " \t") >> string "/*")
    _ <- manyTill anyChar $ try $ string "*/"
    -- FIXME This requires that any line beginning with a comment is entirely a comment
    skipMany $ oneOf " \t"
    _ <- eol <|> eof
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
parseContent allowColon =
    parseHash' <|> parseAt' <|> parseComment <|> parseChar
  where
    parseHash' = either ContentRaw ContentVar `fmap` parseHash
    parseAt' =
        either ContentRaw go `fmap` parseAt
      where
        go (d, False) = ContentUrl d
        go (d, True) = ContentUrlParam d
    parseChar = (ContentRaw . return) `fmap` noneOf restricted
    restricted = (if allowColon then id else (:) ':') "\r\n"
    parseComment = do
        _ <- try $ string "/*"
        _ <- manyTill anyChar $ try $ string "*/"
        return $ ContentRaw ""

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
    [|fromLazyText . toCss|] `appE` return (derefToExp [] d)
contentToBuilder r (ContentUrl u) =
    [|fromText . TS.pack|] `appE`
        (varE r `appE` return (derefToExp [] u) `appE` listE [])
contentToBuilder r (ContentUrlParam u) =
    [|fromText . TS.pack|] `appE`
        ([|uncurry|] `appE` varE r `appE` return (derefToExp [] u))

cassiusFile :: FilePath -> Q Exp
cassiusFile fp = do
    contents <- fmap TL.unpack $ qRunIO $ readUtf8File fp
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
    return $ TupE [d', c' `AppE` derefToExp [] d]
  where
    c :: VarType -> Q Exp
    c VTPlain = [|CDPlain . toCss|]
    c VTUrl = [|CDUrl|]
    c VTUrlParam = [|CDUrlParam|]

cassiusFileDebug :: FilePath -> Q Exp
cassiusFileDebug fp = do
    s <- fmap TL.unpack $ qRunIO $ readUtf8File fp
    let a = either (error . show) id $ parse parseBlocks s s
    c <- mapM vtToExp $ concatMap getVars $ concatMap go a
    cr <- [|cassiusRuntime|]
    return $ cr `AppE` (LitE $ StringL fp) `AppE` ListE c
  where
    go (x, y) = x ++ concatMap go' y
    go' (k, v) = k ++ v

cassiusRuntime :: FilePath -> [(Deref, CDData url)] -> Cassius url
cassiusRuntime fp cd render' = unsafePerformIO $ do
    s <- fmap TL.unpack $ qRunIO $ readUtf8File fp
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


-- CSS size wrappers

-- | Create a CSS size, e.g. $(mkSize "100px").
mkSize :: String -> ExpQ
mkSize s = appE nameE valueE
  where [(value, unit)] = reads s :: [(Double, String)]
        absoluteSizeE = varE $ mkName "absoluteSize"
        nameE = case unit of
          "cm" -> appE absoluteSizeE (conE $ mkName "Centimeter")
          "em" -> conE $ mkName "EmSize"
          "ex" -> conE $ mkName "ExSize"
          "in" -> appE absoluteSizeE (conE $ mkName "Inch")
          "mm" -> appE absoluteSizeE (conE $ mkName "Millimeter")
          "pc" -> appE absoluteSizeE (conE $ mkName "Pica")
          "pt" -> appE absoluteSizeE (conE $ mkName "Point")
          "px" -> conE $ mkName "PixelSize"
          "%" -> varE $ mkName "percentageSize"
        valueE = litE $ rationalL (toRational value)

-- | Absolute size units.
data AbsoluteUnit = Centimeter
                  | Inch
                  | Millimeter
                  | Pica
                  | Point
                  deriving (Eq, Show)

-- | Not intended for direct use, see 'mkSize'.
data AbsoluteSize = AbsoluteSize
    { absoluteSizeUnit :: AbsoluteUnit -- ^ Units used for text formatting.
    , absoluteSizeValue :: Rational -- ^ Normalized value in centimeters.
    }

-- | Absolute size unit convertion rate to centimeters.
absoluteUnitRate :: AbsoluteUnit -> Rational
absoluteUnitRate Centimeter = 1
absoluteUnitRate Inch = 2.54
absoluteUnitRate Millimeter = 0.1
absoluteUnitRate Pica = 12 * absoluteUnitRate Point
absoluteUnitRate Point = 1 / 72 * absoluteUnitRate Inch

-- | Constructs 'AbsoluteSize'. Not intended for direct use, see 'mkSize'.
absoluteSize :: AbsoluteUnit -> Rational -> AbsoluteSize
absoluteSize unit value = AbsoluteSize unit (value * absoluteUnitRate unit)

instance Show AbsoluteSize where
  show (AbsoluteSize unit value') = printf "%f" value ++ suffix
    where value = fromRational (value' / absoluteUnitRate unit) :: Double
          suffix = case unit of
            Centimeter -> "cm"
            Inch -> "in"
            Millimeter -> "mm"
            Pica -> "pc"
            Point -> "pt"

instance Eq AbsoluteSize where
  (AbsoluteSize _ v1) == (AbsoluteSize _ v2) = v1 == v2

instance Ord AbsoluteSize where
  compare (AbsoluteSize _ v1) (AbsoluteSize _ v2) = compare v1 v2

instance Num AbsoluteSize where
  (AbsoluteSize u1 v1) + (AbsoluteSize _ v2) = AbsoluteSize u1 (v1 + v2)
  (AbsoluteSize u1 v1) * (AbsoluteSize _ v2) = AbsoluteSize u1 (v1 * v2)
  (AbsoluteSize u1 v1) - (AbsoluteSize _ v2) = AbsoluteSize u1 (v1 - v2)
  abs (AbsoluteSize u v) = AbsoluteSize u (abs v)
  signum (AbsoluteSize u v) = AbsoluteSize u (abs v)
  fromInteger x = AbsoluteSize Centimeter (fromInteger x)

instance Fractional AbsoluteSize where
  (AbsoluteSize u1 v1) / (AbsoluteSize _ v2) = AbsoluteSize u1 (v1 / v2)
  fromRational x = AbsoluteSize Centimeter (fromRational x)

instance ToCss AbsoluteSize where
  toCss = TL.pack . show

-- | Not intended for direct use, see 'mkSize'.
data PercentageSize = PercentageSize
    { percentageSizeValue :: Rational -- ^ Normalized value, 1 == 100%.
    }
                    deriving (Eq, Ord)

-- | Constructs 'PercentageSize'. Not intended for direct use, see 'mkSize'.
percentageSize :: Rational -> PercentageSize
percentageSize value = PercentageSize (value / 100)

instance Show PercentageSize where
  show (PercentageSize value') = printf "%f" value ++ "%"
    where value = fromRational (value' * 100) :: Double

instance Num PercentageSize where
  (PercentageSize v1) + (PercentageSize v2) = PercentageSize (v1 + v2)
  (PercentageSize v1) * (PercentageSize v2) = PercentageSize (v1 * v2)
  (PercentageSize v1) - (PercentageSize v2) = PercentageSize (v1 - v2)
  abs (PercentageSize v) = PercentageSize (abs v)
  signum (PercentageSize v) = PercentageSize (abs v)
  fromInteger x = PercentageSize (fromInteger x)

instance Fractional PercentageSize where
  (PercentageSize v1) / (PercentageSize v2) = PercentageSize (v1 / v2)
  fromRational x = PercentageSize (fromRational x)

instance ToCss PercentageSize where
  toCss = TL.pack . show

-- | Converts number and unit suffix to CSS format.
showSize :: Rational -> String -> String
showSize value' unit = printf "%f" value ++ unit
  where value = fromRational value' :: Double

mkSizeType "EmSize" "em"
mkSizeType "ExSize" "ex"
mkSizeType "PixelSize" "px"
