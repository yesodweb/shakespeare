module Old.Cassius
    ( oldToNew
    ) where

import Text.ParserCombinators.Parsec hiding (Line)
import Data.List (intercalate)
import Data.Char (isUpper, isDigit)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Blaze.ByteString.Builder (Builder, fromByteString, toLazyByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import Data.Word (Word8)
import Data.Bits
import System.IO.Unsafe (unsafePerformIO)
import Old.Utf8
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL

oldToNew s = concatMap renderBlock $ either (error . show) id $ parse parseBlocks s s

renderBlock (sel, attrs) = concat
    [ renderConts sel
    , "\n"
    , concatMap renderPair attrs
    ]

renderPair (x, y) = concat
    [ "    "
    , renderConts x
    , ": "
    , renderConts y
    , "\n"
    ]

renderConts = concatMap render

render (ContentRaw s) = s
render (ContentVar deref) = concat [ "#{", renderDeref deref, "}" ]
render (ContentUrl deref) = concat [ "@{", renderDeref deref, "}" ]
render (ContentUrlParam deref) = concat [ "@?{", renderDeref deref, "}" ]

renderDeref (DerefLeaf s) = s
renderDeref (DerefBranch x (DerefLeaf y)) = concat [renderDeref x, " ", y]
renderDeref (DerefBranch x y) = concat [renderDeref x, " (", renderDeref y, ")"]

data Deref = DerefLeaf String
           | DerefBranch Deref Deref
    deriving (Show, Eq)

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
