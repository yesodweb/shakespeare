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
    , Html
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
    , HamletScalar (..)
    , HamletException (..)
    , parseHamletRT
    , renderHamletRT
    ) where

import Text.Hamlet.Parse
import Text.Hamlet.Quasi
import Text.Hamlet.RT
import Text.Hamlet.Debug
import Text.Blaze
import qualified Data.ByteString.Lazy as L
import Data.Monoid (mappend)

-- | An function generating an 'Html' given a URL-rendering function.
type Hamlet url = (url -> String) -> Html ()

-- | Converts a 'Hamlet' to lazy bytestring.
renderHamlet :: (url -> String) -> Hamlet url -> L.ByteString
renderHamlet render h = renderHtml $ h render

-- | Wrap an 'Html' for embedding in an XML file.
cdata :: Html () -> Html ()
cdata h =
    preEscapedString "<![CDATA["
    `mappend`
    h
    `mappend`
    preEscapedString "]]>"
