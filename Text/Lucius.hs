{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
module Text.Lucius
    ( -- * Datatypes
      Lucius
      -- * Rendering
    , renderLucius
      -- * Parsing
    , lucius
      -- * Re-export cassius
    , module Text.Cassius
    ) where

import Text.Cassius hiding (Cassius, renderCassius, cassius, cassiusFile, cassiusFileDebug)
import Text.Shakespeare
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Data.Text.Lazy.Builder (fromText)
import Data.Monoid
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Text.Cassius as C
import Text.ParserCombinators.Parsec hiding (Line)
import Text.Css
import Data.Char (isSpace)

type Lucius a = C.Cassius a

renderLucius :: (url -> [(TS.Text, TS.Text)] -> TS.Text)
             -> Lucius url
             -> TL.Text
renderLucius = C.renderCassius

lucius :: QuasiQuoter
lucius = QuasiQuoter { quoteExp = luciusFromString }

luciusFromString :: String -> Q Exp
luciusFromString s =
    blocksToLucius
  $ either (error . show) id $ parse (parseBlocks id) s s

type Block = (Selector, Pairs)

type Pairs = [Pair]

type Pair = (Contents, Contents)

type Selector = Contents

blocksToLucius :: [Block] -> Q Exp
blocksToLucius blocks = do
    r <- newName "_render"
    lamE [varP r] $ listE $ map (blockToCss r) blocks

blockToCss :: Name -> Block -> Q Exp
blockToCss r (sel, pairs) = do
    css' <- [|Css'|]
    let sel' = contentsToBuilder r sel
    props' <- listE (map go pairs)
    return css' `appE` sel' `appE` return props'
  where
    go (x, y) = tupE [contentsToBuilder r x, contentsToBuilder r y]

data Content = ContentRaw String
             | ContentVar Deref
             | ContentUrl Deref
             | ContentUrlParam Deref
    deriving (Show, Eq)
type Contents = [Content]

contentsToBuilder :: Name -> [Content] -> Q Exp
contentsToBuilder r contents =
    appE [|mconcat|] $ listE $ map (contentToBuilder r) contents

contentToBuilder :: Name -> Content -> Q Exp
contentToBuilder _ (ContentRaw x) =
    [|fromText . TS.pack|] `appE` litE (StringL x)
contentToBuilder _ (ContentVar d) =
    [|toCss|] `appE` return (derefToExp [] d)
contentToBuilder r (ContentUrl u) =
    [|fromText|] `appE`
        (varE r `appE` return (derefToExp [] u) `appE` listE [])
contentToBuilder r (ContentUrlParam u) =
    [|fromText|] `appE`
        ([|uncurry|] `appE` varE r `appE` return (derefToExp [] u))

parseBlocks :: ([Block] -> [Block]) -> Parser [Block]
parseBlocks front = do
    whiteSpace
    (parseBlock >>= (\b -> parseBlocks (front . (:) b))) <|> (return $ map compressBlock $ front [])

compressBlock :: Block -> Block
compressBlock = id -- FIXME

whiteSpace :: Parser ()
whiteSpace = many (oneOf " \t\n\r" >> return ()) >> return () -- FIXME comments, don't use many

parseBlock :: Parser Block
parseBlock = do
    sel <- parseSelector
    _ <- char '{'
    whiteSpace
    pairs <- parsePairs id
    whiteSpace
    return (sel, pairs)

parseSelector :: Parser Selector
parseSelector = fmap trim $ parseContents "{"

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

parsePairs :: ([Pair] -> [Pair]) -> Parser [Pair]
parsePairs front = (char '}' >> return (front [])) <|> (do
    x <- parsePair
    parsePairs $ front . (:) x)

parsePair :: Parser Pair
parsePair = do
    key <- parseContents ":"
    _ <- char ':'
    whiteSpace
    val <- parseContents ";}"
    (char ';' >> return ()) <|> return ()
    whiteSpace
    return (key, val)

parseContents :: String -> Parser Contents
parseContents = many1 . parseContent

parseContent :: String -> Parser Content
parseContent restricted =
    parseHash' <|> parseAt' <|> parseComment <|> parseChar
  where
    parseHash' = either ContentRaw ContentVar `fmap` parseHash
    parseAt' =
        either ContentRaw go `fmap` parseAt
      where
        go (d, False) = ContentUrl d
        go (d, True) = ContentUrlParam d
    parseChar = (ContentRaw . return) `fmap` noneOf restricted
    parseComment = do
        _ <- try $ string "/*"
        _ <- manyTill anyChar $ try $ string "*/"
        return $ ContentRaw ""
