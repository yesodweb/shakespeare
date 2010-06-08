{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Text.Hamlet.Monad
    ( -- * Datatypes
      Hamlet (..)
      -- * Output
    , outputUrl
    , outputUrlParams
      -- * Utility functions
    , condH
    , maybeH
    , renderHamlet
    , cdata
    ) where

import Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import Data.List
import Data.ByteString.UTF8 (fromString)
import Text.Blaze

type Hamlet url = (url -> String) -> Html

-- | Wrap some 'HtmlContent' for embedding in an XML file.
cdata :: Html -> Html
cdata h = mconcat
    [ preEscapedString "<![CDATA["
    , h
    , preEscapedString "]]>"
    ]

-- | Uses the URL rendering function to convert the given URL to a 'String' and
-- then calls 'outputString'.
outputUrl :: (url -> String) -> url -> Html
outputUrl render u = outputString $ render u

-- | Same as 'outputUrl', but appends a query-string with given keys and
-- values.
outputUrlParams :: (url -> String) -> (url, [(String, String)]) -> Html
outputUrlParams render (u, []) = outputUrl render u
outputUrlParams render (u, params) = mappend
    (outputUrl render u)
    (outputString $ showParams params)
  where
    showParams x = '?' : intercalate "&" (map go x)
    go (x, y) = go' x ++ '=' : go' y
    go' = concatMap encodeUrlChar

-- | Taken straight from web-encodings; reimplemented here to avoid extra
-- dependencies.
encodeUrlChar :: Char -> String
encodeUrlChar c
    -- List of unreserved characters per RFC 3986
    -- Gleaned from http://en.wikipedia.org/wiki/Percent-encoding
    | 'A' <= c && c <= 'Z' = [c]
    | 'a' <= c && c <= 'z' = [c]
    | '0' <= c && c <= '9' = [c]
encodeUrlChar c@'-' = [c]
encodeUrlChar c@'_' = [c]
encodeUrlChar c@'.' = [c]
encodeUrlChar c@'~' = [c]
encodeUrlChar ' ' = "+"
encodeUrlChar y =
    let (a, c) = fromEnum y `divMod` 16
        b = a `mod` 16
        showHex' x
            | x < 10 = toEnum $ x + (fromEnum '0')
            | x < 16 = toEnum $ x - 10 + (fromEnum 'A')
            | otherwise = error $ "Invalid argument to showHex: " ++ show x
     in ['%', showHex' b, showHex' c]

-- | Checks for truth in the left value in each pair in the first argument. If
-- a true exists, then the corresponding right action is performed. Only the
-- first is performed. In there are no true values, then the second argument is
-- performed, if supplied.
condH :: [(Bool, Html)] -- FIXME could probably just be a foldr
      -> Maybe Html
      -> Html
condH [] Nothing = mempty
condH [] (Just x) = x
condH ((True, y):_) _ = y
condH ((False, _):rest) z = condH rest z

-- | Runs the second argument with the value in the first, if available.
-- Otherwise, runs the third argument, if available.
maybeH :: Maybe v
       -> (v -> Html)
       -> Maybe Html
       -> Html
maybeH Nothing _ Nothing = mempty
maybeH Nothing _ (Just x) = x
maybeH (Just v) f _ = f v

-- | Converts a 'Hamlet' to lazy text, using strict I/O.
renderHamlet :: (url -> String) -> Hamlet url -> L.ByteString
renderHamlet render h = renderHtml $ h render

encodeHtmlChar :: Char -> String
encodeHtmlChar '<' = "&lt;"
encodeHtmlChar '>' = "&gt;"
encodeHtmlChar '&' = "&amp;"
encodeHtmlChar '"' = "&quot;"
encodeHtmlChar '\'' = "&#39;"
encodeHtmlChar c = [c]
