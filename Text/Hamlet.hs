{-# LANGUAGE OverloadedStrings #-}
module Text.Hamlet
    ( -- * Basic quasiquoters
      hamlet
    , xhamlet
    , hamletDebug
      -- * Load from external file
    , hamletFile
    , xhamletFile
    , hamletFileDebug
      -- * Customized settings
    , hamletWithSettings
    , hamletFileWithSettings
    , HamletSettings (..)
    , defaultHamletSettings
    , xhtmlHamletSettings
      -- * Datatypes
    , Html (..)
    , Hamlet
      -- * Typeclass
    , ToHtml (..)
    , HamletValue (..)
      -- * Construction
    , preEscapedString
    , string
    , unsafeByteString
    , cdata
      -- * Rendering
      -- ** ByteString
    , renderHamlet
    , renderHtml
      -- ** Text
    , renderHamletText
    , renderHtmlText
      -- * Runtime Hamlet
    , HamletRT
    , HamletData (..)
    , HamletException (..)
    , parseHamletRT
    , renderHamletRT
    ) where

import Text.Hamlet.Parse
import Text.Hamlet.Quasi
import Text.Hamlet.RT
import Text.Hamlet.Debug
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Monoid (mappend)
import Blaze.ByteString.Builder (toLazyByteString, fromByteString)
import Blaze.ByteString.Builder.Html.Utf8 (fromHtmlEscapedString)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Encoding.Error as T

-- | Converts a 'Hamlet' to lazy bytestring.
renderHamlet :: (url -> [(String, String)] -> String) -> Hamlet url -> L.ByteString
renderHamlet render h = renderHtml $ h render

renderHamletText :: (url -> [(String, String)] -> String) -> Hamlet url
                 -> T.Text
renderHamletText render h =
    T.decodeUtf8With T.lenientDecode $ renderHtml $ h render

renderHtml :: Html -> L.ByteString
renderHtml (Html h) = toLazyByteString h

renderHtmlText :: Html -> T.Text
renderHtmlText (Html h) = T.decodeUtf8With T.lenientDecode $ toLazyByteString h

-- | Wrap an 'Html' for embedding in an XML file.
cdata :: Html -> Html
cdata h =
    Html (fromByteString "<![CDATA[")
    `mappend`
    h
    `mappend`
    Html (fromByteString "]]>")

preEscapedString :: String -> Html
preEscapedString = Html . fromString

string :: String -> Html
string = Html . fromHtmlEscapedString

unsafeByteString :: S.ByteString -> Html
unsafeByteString = Html . fromByteString
