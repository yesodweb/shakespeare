module Text.Css
    ( Css' (..)
    ) where

import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy as TL

data Css' = Css'
    { _cssSelectors :: Builder
    , _cssAttributes :: [(Builder, Builder)]
    }
