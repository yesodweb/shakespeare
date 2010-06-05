module Text.Hamlet
    ( hamlet
    , xhamlet
    , hamletWithSettings
    , HamletSettings (..)
    , defaultHamletSettings
    , Hamlet (..)
    , HtmlContent (..)
    , printHamlet
    ) where

import Text.Hamlet.Monad
import Text.Hamlet.Parse
import Text.Hamlet.Quasi
