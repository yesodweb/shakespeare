module Old.Julius
    ( oldToNew
    ) where

import Text.ParserCombinators.Parsec hiding (Line)
import Data.Char (isUpper, isDigit)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import System.IO.Unsafe (unsafePerformIO)
import Old.Utf8
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL

oldToNew s = concatMap render $ either (error . show) id $ parse parseContents s s

render (ContentRaw s) = s
render (ContentVar deref) = concat [ "#{", renderDeref deref, "}" ]
render (ContentUrl deref) = concat [ "@{", renderDeref deref, "}" ]
render (ContentUrlParam deref) = concat [ "@?{", renderDeref deref, "}" ]
render (ContentMix deref) = concat [ "^{", renderDeref deref, "}" ]

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
