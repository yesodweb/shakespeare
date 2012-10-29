{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Text.Cassius
    ( -- * Datatypes
      Css
    , CssUrl
      -- * Type class
    , ToCss (..)
      -- * Rendering
    , renderCss
    , renderCssUrl
      -- * Parsing
    , cassius
    , cassiusFile
    , cassiusFileDebug
    , cassiusFileReload
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
    ) where

import Text.Css
import Text.Shakespeare.Base
import Text.ParserCombinators.Parsec hiding (Line)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Data.Maybe (catMaybes)
import qualified Data.Text.Lazy as TL
import Data.Char (isSpace)
import Text.CssCommon

parseBlocks :: Parser [Block]
parseBlocks = (map compressBlock . catMaybes) `fmap` many parseBlock

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
    sum `fmap` many ((char ' ' >> return 1) <|> (char '\t' >> fail "Tabs are not allowed in Cassius indentation"))

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
            _ -> return $ Just $ Block [name] pairs []
    parsePair' indent = try (parseEmptyLine >> return Nothing)
                    <|> try (Just `fmap` parsePair indent)

parsePair :: Int -> Parser (Contents, Contents)
parsePair minIndent = do
    indent <- parseIndent
    if indent <= minIndent then fail "not indented" else return ()
    key <- manyTill (parseContent False) $ char ':'
    spaces
    value <- manyTill (parseContent True) $ eol <|> eof
    return (trim key, value) -- FIXME consider trimming value as well

trim :: Contents -> Contents
trim =
    reverse . go . reverse . go
  where
    go [] = []
    go (ContentRaw x:xs) =
        case dropWhile isSpace x of
            [] -> go xs
            y -> ContentRaw y:xs
    go x = x


eol :: Parser ()
eol = (char '\n' >> return ()) <|> (string "\r\n" >> return ())

parseContent :: Bool -> Parser Content
parseContent allowColon =
    parseHash' <|> parseAt' <|> parseComment' <|> parseChar
  where
    parseHash' = either ContentRaw ContentVar `fmap` parseHash
    parseAt' =
        either ContentRaw go `fmap` parseAt
      where
        go (d, False) = ContentUrl d
        go (d, True) = ContentUrlParam d
    parseChar = (ContentRaw . return) `fmap` noneOf restricted
    restricted = (if allowColon then id else (:) ':') "\r\n"
    parseComment' = do
        _ <- try $ string "/*"
        _ <- manyTill anyChar $ try $ string "*/"
        return $ ContentRaw ""

cassius :: QuasiQuoter
cassius = QuasiQuoter { quoteExp = cassiusFromString }

cassiusFromString :: String -> Q Exp
cassiusFromString s =
    topLevelsToCassius $ map TopBlock
  $ either (error . show) id $ parse parseBlocks s s

cassiusFile :: FilePath -> Q Exp
cassiusFile fp = do
#ifdef GHC_7_4
    qAddDependentFile fp
#endif
    contents <- fmap TL.unpack $ qRunIO $ readUtf8File fp
    cassiusFromString contents

cassiusFileDebug, cassiusFileReload :: FilePath -> Q Exp
cassiusFileDebug = cssFileDebug [|parseTopLevels|] parseTopLevels
cassiusFileReload = cassiusFileDebug

parseTopLevels :: Parser [TopLevel]
parseTopLevels = do
    x <- parseBlocks
    return $ map TopBlock x
