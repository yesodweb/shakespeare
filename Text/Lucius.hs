{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Text.Lucius
    ( -- * Parsing
      lucius
    , luciusOrd
    , luciusFile
    , luciusFileOrd
    , luciusFileDebug
    , luciusFileDebugOrd
    , luciusFileReload
    , luciusFileReloadOrd
      -- ** Mixins
    , luciusMixin
    , luciusMixinOrd
    , Mixin
      -- ** Runtime
    , luciusRT
    , luciusRTOrd
    , luciusRT'
    , luciusRTOrd'
    , luciusRTMinified
    , luciusRTMinifiedOrd
      -- *** Mixin
    , luciusRTMixin
    , luciusRTMixinOrd
    , RTValue (..)
    , -- * Datatypes
      Css
    , CssUrl
      -- * Type class
    , ToCss (..)
      -- * Rendering
    , renderCss
    , renderCssUrl
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
      -- * Internal
    , parseTopLevels
    , luciusUsedIdentifiers
    ) where

import Text.Internal.CssCommon
import Text.Shakespeare.Base
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Data.Text (Text, unpack)
import qualified Data.Text.Lazy as TL
import Text.ParserCombinators.Parsec hiding (Line)
import Text.Internal.Css
import Data.Char (isSpace, toLower, toUpper)
import Numeric (readHex)
import Control.Applicative ((<$>))
import Control.Monad (when, unless)
import Data.Monoid (mconcat)
import Data.List (isSuffixOf)
import Control.Arrow (second)
import Text.Shakespeare (VarType)

-- |
--
-- >>> renderCss ([lucius|foo{bar:baz}|] undefined)
-- "foo{bar:baz}"
lucius :: QuasiQuoter
lucius = luciusWithOrder Unordered
{-# DEPRECATED lucius "Use 'luciusOrd' instead" #-}

-- | Like 'lucius' but preserves the order of attributes and mixins
-- @since 2.0.28
luciusOrd :: QuasiQuoter
luciusOrd = luciusWithOrder Ordered

luciusWithOrder :: Order -> QuasiQuoter
luciusWithOrder order = QuasiQuoter { quoteExp = luciusFromString order}

luciusFromString :: Order -> String -> Q Exp
luciusFromString order s =
    topLevelsToCassius
  $ either (error . show) id $ parse (parseTopLevels order) s s

whiteSpace :: Parser ()
whiteSpace = many whiteSpace1 >> return ()

whiteSpace1 :: Parser ()
whiteSpace1 =
    ((oneOf " \t\n\r" >> return ()) <|> (parseComment >> return ()))

parseBlock :: Order -> Parser (Block 'Unresolved)
parseBlock order = do
    sel <- parseSelector
    _ <- char '{'
    whiteSpace
    pairsBlocks <- parsePairsBlocks order id
    let (attrs, blocks, mixins) = case order of
          Unordered -> partitionPBs  pairsBlocks
          Ordered   -> partitionPBs' pairsBlocks
    whiteSpace
    return $ BlockUnresolved sel attrs (map detectAmp blocks) mixins

-- | Looks for an & at the beginning of a selector and, if present, indicates
-- that we should not have a leading space. Otherwise, we should have the
-- leading space.
detectAmp :: Block 'Unresolved -> (Bool, Block 'Unresolved)
detectAmp (BlockUnresolved (sel) b c d) =
    (hls, BlockUnresolved sel' b c d)
  where
    (hls, sel') =
        case sel of
            (ContentRaw "&":rest):others -> (False, rest : others)
            (ContentRaw ('&':s):rest):others -> (False, (ContentRaw s : rest) : others)
            _ -> (True, sel)

-- | This function implementation does not keep the order
-- of attributes and mixins and is used in legacy parsers:
-- 'cassius', 'cassiusFile', 'lucius', 'luciusFile' etc.
-- To maintain attributes and mixins ordering, use 'partitionPBs'.
partitionPBs :: [PairBlock]
              -> ([Either (Attr 'Unresolved) Deref], [Block 'Unresolved], [Deref])
partitionPBs =
    go id id id
  where
    go a b c [] = (a [], b [], c [])
    go a b c (PBAttr x:xs) = go (a . ((Left x):)) b c xs
    go a b c (PBBlock x:xs) = go a (b . (x:)) c xs
    go a b c (PBMixin x:xs) = go a b (c . (x:)) xs

partitionPBs' :: [PairBlock]
             -> ([Either (Attr 'Unresolved) Deref], [Block 'Unresolved], [Deref])
partitionPBs' =
    go id id
  where
    go a b [] = (a [], b [], [])
    go a b (PBAttr x:xs) = go (a . ((Left x):)) b xs
    go a b (PBMixin x:xs) = go (a . ((Right x):)) b xs
    go a b (PBBlock x:xs) = go a (b . (x:)) xs

parseSelector :: Parser [Contents]
parseSelector =
    go id
  where
    go front = do
        c <- parseContents "{,"
        let front' = front . (:) (trim c)
        (char ',' >> go front') <|> return (front' [])

trim :: Contents -> Contents
trim =
    reverse . trim' False . reverse . trim' True
  where
    trim' _ [] = []
    trim' b (ContentRaw s:rest) =
        let s' = trimS b s
         in if null s' then trim' b rest else ContentRaw s' : rest
    trim' _ x = x
    trimS True = dropWhile isSpace
    trimS False = reverse . dropWhile isSpace . reverse

data PairBlock = PBAttr (Attr 'Unresolved)
               | PBBlock (Block 'Unresolved)
               | PBMixin Deref
parsePairsBlocks :: Order -> ([PairBlock] -> [PairBlock]) -> Parser [PairBlock]
parsePairsBlocks order front = (char '}' >> return (front [])) <|> (do
    isBlock <- lookAhead checkIfBlock
    x <- grabMixin <|> (if isBlock then grabBlock else grabPair)
    parsePairsBlocks order $ front . (:) x)
  where
    grabBlock = do
        b <- parseBlock order
        whiteSpace
        return $ PBBlock b
    grabPair = PBAttr <$> parsePair
    grabMixin = try $ do
        whiteSpace
        Right x <- parseCaret
        whiteSpace
        (char ';' >> return ()) <|> return ()
        whiteSpace
        return $ PBMixin x
    checkIfBlock = do
        skipMany $ noneOf "#@{};"
        (parseHash >> checkIfBlock)
            <|> (parseAt >> checkIfBlock)
            <|> (char '{' >> return True)
            <|> (oneOf ";}" >> return False)
            <|> (anyChar >> checkIfBlock)
            <|> fail "checkIfBlock"

parsePair :: Parser (Attr 'Unresolved)
parsePair = do
    key <- parseContents ":"
    _ <- char ':'
    whiteSpace
    val <- parseContents ";}"
    (char ';' >> return ()) <|> return ()
    whiteSpace
    return $ AttrUnresolved key val

parseContents :: String -> Parser Contents
parseContents = many1 . parseContent

parseContent :: String -> Parser Content
parseContent restricted =
    parseHash' <|> parseAt' <|> parseComment <|> parseBack <|> parseChar
  where
    parseHash' = either ContentRaw ContentVar `fmap` parseHash
    parseAt' =
        either ContentRaw go `fmap` parseAt
      where
        go (d, False) = ContentUrl d
        go (d, True) = ContentUrlParam d
    parseBack = try $ do
        _ <- char '\\'
        hex <- atMost 6 $ satisfy isHex
        (int, _):_ <- return $ readHex $ dropWhile (== '0') hex
        when (length hex < 6) $
            ((string "\r\n" >> return ()) <|> (satisfy isSpace >> return ()))
        return $ ContentRaw [toEnum int]
    parseChar = (ContentRaw . return) `fmap` noneOf restricted

isHex :: Char -> Bool
isHex c =
    ('0' <= c && c <= '9') ||
    ('A' <= c && c <= 'F') ||
    ('a' <= c && c <= 'f')

atMost :: Int -> Parser a -> Parser [a]
atMost 0 _ = return []
atMost i p = (do
    c <- p
    s <- atMost (i - 1) p
    return $ c : s) <|> return []

parseComment :: Parser Content
parseComment = do
    _ <- try $ string "/*"
    _ <- manyTill anyChar $ try $ string "*/"
    return $ ContentRaw ""

luciusFile :: FilePath -> Q Exp
luciusFile = luciusFileWithOrd Unordered
{-# DEPRECATED luciusFile "Use 'luciusFileOrd' instead" #-}

-- | Like 'luciusFile' but preserves the order of attributes and mixins
-- @since 2.0.28
luciusFileOrd :: FilePath -> Q Exp
luciusFileOrd = luciusFileWithOrd Ordered

luciusFileWithOrd :: Order -> FilePath -> Q Exp
luciusFileWithOrd order fp = do
    contents <- readFileRecompileQ fp
    luciusFromString order contents

luciusFileDebug :: FilePath -> Q Exp
luciusFileDebug = luciusFileDebugWithOrder Unordered
{-# DEPRECATED luciusFileDebug "Use 'luciusFileDebugOrd' instead" #-}

luciusFileReload :: FilePath -> Q Exp
luciusFileReload = luciusFileDebug
{-# DEPRECATED luciusFileReload "Use 'luciusFileReloadOrd' instead" #-}

-- | Like 'luciusFileDebug' but preserves the order of attributes and mixins
-- @since 2.0.28
luciusFileDebugOrd :: FilePath -> Q Exp
luciusFileDebugOrd = luciusFileDebugWithOrder Ordered

-- | Like 'luciusFileReload' but preserves the order of attributes and mixins
-- @since 2.0.28
luciusFileReloadOrd :: FilePath -> Q Exp
luciusFileReloadOrd = luciusFileDebugOrd

luciusFileDebugWithOrder :: Order -> FilePath -> Q Exp
luciusFileDebugWithOrder order =
  cssFileDebug False [|parseTopLevels order|] (parseTopLevels order)

parseTopLevels :: Order -> Parser [TopLevel 'Unresolved]
parseTopLevels order =
    go id
  where
    go front = do
        let string' s = string s >> return ()
            ignore = many (whiteSpace1 <|> string' "<!--" <|> string' "-->")
                        >> return ()
        ignore
        tl <- ((charset <|> media <|> impor <|> supports <|> topAtBlock <|> var <|> fmap TopBlock (parseBlock order)) >>= \x -> go (front . (:) x))
            <|> (return $ map compressTopLevel $ front [])
        ignore
        return tl
    charset = do
        try $ stringCI "@charset "
        cs <- parseContents ";"
        _ <- char ';'
        return $ TopAtDecl "charset" cs
    media = do
        try $ stringCI "@media "
        selector <- parseContents "{"
        _ <- char '{'
        b <- parseBlocks id
        return $ TopAtBlock "media" selector b
    impor = do
        try $ stringCI "@import ";
        val <- parseContents ";"
        _ <- char ';'
        return $ TopAtDecl "import" val
    supports = do
        try $ stringCI "@supports "
        selector <- parseContents "{"
        _ <- char '{'
        b <- parseBlocks id
        return $ TopAtBlock "supports" selector b
    var = try $ do
        _ <- char '@'
        isPage <- (try $ string "page " >> return True) <|>
                  (try $ string "font-face " >> return True) <|>
                    return False
        when isPage $ fail "page is not a variable"
        k <- many1 $ noneOf ":"
        _ <- char ':'
        v <- many1 $ noneOf ";"
        _ <- char ';'
        let trimS = reverse . dropWhile isSpace . reverse . dropWhile isSpace
        return $ TopVar (trimS k) (trimS v)
    topAtBlock = do
        (name, selector) <- try $ do
            _ <- char '@'
            name <- many1 $ noneOf " \t"
            _ <- many1 $ oneOf " \t"
            unless ("keyframes" `isSuffixOf` name) $ fail "only accepting keyframes"
            selector <- parseContents "{"
            _ <- char '{'
            return (name, selector)
        b <- parseBlocks id
        return $ TopAtBlock name selector b
    parseBlocks front = do
        whiteSpace
        (char '}' >> return (map compressBlock $ front []))
            <|> ((parseBlock order) >>= \x -> parseBlocks (front . (:) x))

stringCI :: String -> Parser ()
stringCI [] = return ()
stringCI (c:cs) = (char (toLower c) <|> char (toUpper c)) >> stringCI cs

luciusRT' :: TL.Text
          -> Either String ([(Text, Text)] -> Either String [TopLevel 'Resolved])
luciusRT' = luciusRTWithOrder' Unordered
{-# DEPRECATED luciusRT' "Use luciusRT' instead" #-}

-- | Like @luciusRT'@ but preserves the order of attributes and mixins
-- @since 2.0.28
luciusRTOrd' :: TL.Text
          -> Either String ([(Text, Text)] -> Either String [TopLevel 'Resolved])
luciusRTOrd' = luciusRTWithOrder' Ordered

luciusRTWithOrder'::
     Order -- ^ Should we keep attributes and mixins order or not
  -> TL.Text
  -> Either String ([(Text, Text)] -> Either String [TopLevel 'Resolved])
luciusRTWithOrder' order =
    either Left (Right . go) . luciusRTInternal order
  where
    go :: ([(Text, RTValue)] -> Either String [TopLevel 'Resolved])
       -> ([(Text, Text)] -> Either String [TopLevel 'Resolved])
    go f = f . map (second RTVRaw)

luciusRTInternal ::
     Order
  -> TL.Text
  -> Either String ([(Text, RTValue)]
  -> Either String [TopLevel 'Resolved])
luciusRTInternal order tl =
    case parse (parseTopLevels order) (TL.unpack tl) (TL.unpack tl) of
        Left s -> Left $ show s
        Right tops -> Right $ \scope -> go scope tops
  where
    go :: [(Text, RTValue)]
       -> [TopLevel 'Unresolved]
       -> Either String [TopLevel 'Resolved]
    go _ [] = Right []
    go scope (TopAtDecl dec cs':rest) = do
        let scope' = map goScope scope
            render = error "luciusRT has no URLs"
        cs <- mapM (contentToBuilderRT scope' render) cs'
        rest' <- go scope rest
        Right $ TopAtDecl dec (mconcat cs) : rest'
    go scope (TopBlock b:rest) = do
        b' <- goBlock scope b
        rest' <- go scope rest
        Right $ map TopBlock b' ++ rest'
    go scope (TopAtBlock name m' bs:rest) = do
        let scope' = map goScope scope
            render = error "luciusRT has no URLs"
        m <- mapM (contentToBuilderRT scope' render) m'
        bs' <- mapM (goBlock scope) bs
        rest' <- go scope rest
        Right $ TopAtBlock name (mconcat m) (concat bs') : rest'
    go scope (TopVar k v:rest) = go ((pack k, RTVRaw $ pack v):scope) rest

    goBlock :: [(Text, RTValue)]
            -> Block 'Unresolved
            -> Either String [Block 'Resolved]
    goBlock scope =
        either Left (Right . ($ [])) . blockRuntime scope' (error "luciusRT has no URLs")
      where
        scope' = map goScope scope

    goScope (k, rt) =
        (DerefIdent (Ident $ unpack k), cd)
      where
        cd =
            case rt of
                RTVRaw t -> CDPlain $ fromText t
                RTVMixin m -> CDMixin m

luciusRT :: TL.Text -> [(Text, Text)] -> Either String TL.Text
luciusRT = luciusRTWithOrder Unordered
{-# DEPRECATED luciusRT "Use 'luciusRTOrd' instead" #-}

-- | Like 'luciusRT' but preserves the order of attributes and mixins
-- @since 2.0.28
luciusRTOrd :: TL.Text -> [(Text, Text)] -> Either String TL.Text
luciusRTOrd = luciusRTWithOrder Ordered

luciusRTWithOrder :: Order -> TL.Text -> [(Text, Text)] -> Either String TL.Text
luciusRTWithOrder order tl scope =
  either Left (Right . renderCss . CssWhitespace) $ either Left ($ scope) (luciusRTWithOrder' order tl)

-- | Runtime Lucius with mixin support.
--
-- Since 1.0.6
luciusRTMixin :: TL.Text -- ^ template
              -> Bool -- ^ minify?
              -> [(Text, RTValue)] -- ^ scope
              -> Either String TL.Text
luciusRTMixin = luciusRTMixinWithOrder Unordered
{-# DEPRECATED luciusRTMixin "Use 'luciusRTMixinOrd' instead" #-}

-- | Like 'luciusRTMixin' but preserves the order of attributes and mixins.
-- @since 2.0.28
luciusRTMixinOrd :: TL.Text -- ^ template
              -> Bool -- ^ minify?
              -> [(Text, RTValue)] -- ^ scope
              -> Either String TL.Text
luciusRTMixinOrd = luciusRTMixinWithOrder Ordered

luciusRTMixinWithOrder ::
     Order
  -> TL.Text -- ^ template
  -> Bool -- ^ minify?
  -> [(Text, RTValue)] -- ^ scope
  -> Either String TL.Text
luciusRTMixinWithOrder order tl minify scope =
    either Left (Right . renderCss . cw) $ either Left ($ scope) (luciusRTInternal order tl)
  where
    cw | minify = CssNoWhitespace
       | otherwise = CssWhitespace

data RTValue = RTVRaw Text
             | RTVMixin Mixin

-- | Same as 'luciusRT', but output has no added whitespace.
--
-- Since 1.0.3
luciusRTMinified :: TL.Text -> [(Text, Text)] -> Either String TL.Text
luciusRTMinified = luciusRTMinifiedWithOrder Unordered
{-# DEPRECATED luciusRTMinified "Use 'luciusRTMinifiedOrd' instead" #-}

-- | Like 'luciusRTMinified' but preserves the order of attributes and mixins.
-- @since 2.0.28
luciusRTMinifiedOrd :: TL.Text -> [(Text, Text)] -> Either String TL.Text
luciusRTMinifiedOrd = luciusRTMinifiedWithOrder Ordered

luciusRTMinifiedWithOrder :: Order -> TL.Text -> [(Text, Text)] -> Either String TL.Text
luciusRTMinifiedWithOrder order tl scope =
  either Left (Right . renderCss . CssNoWhitespace) $ either Left ($ scope) (luciusRTWithOrder' order tl)

-- | Determine which identifiers are used by the given template, useful for
-- creating systems like yesod devel.
luciusUsedIdentifiers :: Order -> String -> [(Deref, VarType)]
luciusUsedIdentifiers order = cssUsedIdentifiers False (parseTopLevels order)

luciusMixin :: QuasiQuoter
luciusMixin = luciusMixinWithOrder Unordered
{-# DEPRECATED luciusMixin "Use 'luciusMixinOrd' instead" #-}

-- | Like 'luciusMixin' but preserves the order of attributes and mixins.
-- @since 2.0.28
luciusMixinOrd :: QuasiQuoter
luciusMixinOrd = luciusMixinWithOrder Ordered

luciusMixinWithOrder :: Order -> QuasiQuoter
luciusMixinWithOrder order = QuasiQuoter { quoteExp = luciusMixinFromString order}

luciusMixinFromString :: Order -> String -> Q Exp
luciusMixinFromString order s' = do
    r <- newName "_render"
    case fmap compressBlock $ parse (parseBlock order) s s of
        Left e -> error $ show e
        Right block -> blockToMixin r [] block
  where
    s = concat ["mixin{", s', "}"]
