-- |
-- Elementary parsers for HTML5, that thoroughly follow the specification
-- (http://dev.w3.org/html5/spec/syntax.html).
module Text.Hamlet.ParseHtml5 (attributeName, tagName) where

import qualified Data.Char as Char
import Text.Parsec
import Text.Parsec.String (Parser)

-- |
-- see http://dev.w3.org/html5/spec/syntax.html#syntax-tag-name
tagName :: Parser String
tagName = many1 $ satisfy isValid
  where
    -- NOTE: This is different from `Char.isAlphaNum`!
    isValid c =
         ('a' <= c && c <= 'z')
      || ('A' <= c && c <= 'Z')
      || ('0' <= c && c <= '9')

-- |
-- see http://dev.w3.org/html5/spec/syntax.html#attributes-0
attributeName :: Parser String
attributeName = many1 $ satisfy (not . isInvalid)
  where
    -- NOTE: In addtion to the requirements that are checked here, the
    -- specification requires to exclude "characters that are not defined by
    -- Unicode".  We do not make any effort to ensure that.
    isInvalid c =
         isSpaceCharacter c
      || Char.isControl c
      || c == '\x0000' -- NULL
      || c == '\x0022' -- QUOTATION MARK (")
      || c == '\x0027' -- APOSTROPHE ('),
      || c == '\x003E' -- GREATER-THAN SIGN (>)
      || c == '\x002F' -- SOLIDUS (/)
      || c == '\x003D' -- EQUALS SIGN (=)

-- |
--
-- NOTE: This is different from `Char.isSpace`!
--
-- see http://dev.w3.org/html5/spec/common-microsyntaxes.html#space-character
isSpaceCharacter :: Char -> Bool
isSpaceCharacter c =
     c == '\x0020' -- SPACE
  || c == '\x0009' -- CHARACTER TABULATION (tab)
  || c == '\x000A' -- LINE FEED (LF)
  || c == '\x000C' -- FORM FEED (FF)
  || c == '\x000D' -- CARRIAGE RETURN (CR)
