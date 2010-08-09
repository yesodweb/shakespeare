{-# LANGUAGE OverloadedStrings #-}
module Text.Hamlet
    ( -- * Basic quasiquoters
      hamlet
    , xhamlet
    , hamlet'
    , xhamlet'
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
      -- * Construction/rendering
    , renderHamlet
    , renderHtml
    , preEscapedString
    , string
    , unsafeByteString
    , cdata
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
import Text.Blaze.Builder.Core (toLazyByteString, fromByteString)
import Text.Blaze.Builder.Html (fromHtmlEscapedString)
import Text.Blaze.Builder.Utf8 (fromString)

-- | Converts a 'Hamlet' to lazy bytestring.
renderHamlet :: (url -> [(String, String)] -> String) -> Hamlet url -> L.ByteString
renderHamlet render h = renderHtml $ h render

renderHtml :: Html -> L.ByteString
renderHtml (Html h) = toLazyByteString h

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
