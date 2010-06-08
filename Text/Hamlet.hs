module Text.Hamlet
    ( hamlet
    , xhamlet
    , hamletWithSettings
    , HamletSettings (..)
    , defaultHamletSettings
    , Hamlet (..)
    , renderHamlet
    , preEscapedString
    , string
    , Html
    ) where

import Text.Hamlet.Monad
import Text.Hamlet.Parse
import Text.Hamlet.Quasi
import Text.Blaze
