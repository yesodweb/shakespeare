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
    , Html
    , Hamlet
      -- * Typeclass
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
import qualified Data.ByteString.Lazy as L
import Data.Monoid (mappend)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Text.Blaze.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Renderer.Text as BT
import Text.Blaze (preEscapedText, preEscapedString, string, unsafeByteString)

-- | Converts a 'Hamlet' to lazy bytestring.
renderHamlet :: (url -> [(String, String)] -> String) -> Hamlet url -> L.ByteString
renderHamlet render h = renderHtml $ h render

renderHamletText :: (url -> [(String, String)] -> String) -> Hamlet url
                 -> T.Text
renderHamletText render h =
    T.decodeUtf8With T.lenientDecode $ renderHtml $ h render

renderHtmlText :: Html -> T.Text
renderHtmlText = BT.renderHtml

-- | Wrap an 'Html' for embedding in an XML file.
cdata :: Html -> Html
cdata h =
    preEscapedText "<![CDATA["
    `mappend`
    h
    `mappend`
    preEscapedText "]]>"
