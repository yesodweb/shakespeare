module Text.Hamlet
    ( hamlet
    , hamletWithSettings
    , HamletSettings (..)
    , defaultHamletSettings
    , Hamlet (..)
    , HtmlContent (..)
    ) where

import Text.Hamlet.Monad
import Text.Hamlet.Parse
import Text.Hamlet.Quasi
