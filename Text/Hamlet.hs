module Text.Hamlet
    ( -- * Basic quasiquoters
      hamlet
    , xhamlet
      -- * Build customized quasiquoters
    , hamletWithSettings
    , HamletSettings (..)
    , defaultHamletSettings
      -- * Datatypes
    , Html
    , Hamlet
      -- * Construction/rendering
    , renderHamlet
    , preEscapedString
    , string
    ) where

import Text.Hamlet.Parse
import Text.Hamlet.Quasi
import Text.Blaze
import qualified Data.ByteString.Lazy as L
import Data.Monoid (mappend)

-- | An function generating an 'Html' given a URL-rendering function.
type Hamlet url = (url -> String) -> Html

-- | Converts a 'Hamlet' to lazy bytestring.
renderHamlet :: (url -> String) -> Hamlet url -> L.ByteString
renderHamlet render h = renderHtml $ h render

-- | Wrap an 'Html' for embedding in an XML file.
cdata :: Html -> Html
cdata h =
    preEscapedString "<![CDATA["
    `mappend`
    h
    `mappend`
    preEscapedString "]]>"
