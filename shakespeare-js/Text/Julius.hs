{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
-- | A Shakespearean module for Javascript templates, introducing type-safe,
-- compile-time variable and url interpolation.--
-- To use this module, @coffee@ must be installed on your system.
--
-- You might consider trying 'Text.Coffee', which compiles down to Javascript.
--
-- Further reading: <http://www.yesodweb.com/book/templates>
--
-- This module is currently in an identity crisis. Originally called Julius, now being changed to just Javascript (shakespeare-javascript)
module Text.Julius
    ( -- * Functions
      -- ** Template-Reading Functions
      -- | These QuasiQuoter and Template Haskell methods return values of
      -- type @'JavascriptUrl' url@. See the Yesod book for details.
      js
    , julius
    , juliusFile
    , jsFile
    , juliusFileDebug
    , jsFileDebug
    , juliusFileReload
    , jsFileReload

      -- * Datatypes
    , JavascriptUrl
    , Javascript (..)

      -- * Typeclass for interpolated variables
    , ToJavascript (..)

      -- ** Rendering Functions
    , renderJavascript
    , renderJavascriptUrl

      -- ** internal, used by 'Text.Coffee'
    , javascriptSettings
    ) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText, fromLazyText)
import Data.Monoid
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Text.Shakespeare

renderJavascript :: Javascript -> TL.Text
renderJavascript (Javascript b) = toLazyText b

-- | render with route interpolation. If using this module standalone, apart
-- from type-safe routes, a dummy renderer can be used:
-- 
-- > renderJavascriptUrl (\_ _ -> undefined) javascriptUrl
--
-- When using Yesod, a renderer is generated for you, which can be accessed
-- within the GHandler monad: 'Yesod.Handler.getUrlRenderParams'.
renderJavascriptUrl :: (url -> [(TS.Text, TS.Text)] -> TS.Text) -> JavascriptUrl url -> TL.Text
renderJavascriptUrl r s = renderJavascript $ s r

-- | Newtype wrapper of 'Builder'.
newtype Javascript = Javascript { unJavascript :: Builder }
    deriving Monoid

-- | Return type of template-reading functions.
type JavascriptUrl url = (url -> [(TS.Text, TS.Text)] -> TS.Text) -> Javascript

-- | A typeclass for types that can be interpolated in CoffeeScript templates.
class ToJavascript a where
    toJavascript :: a -> Builder
instance ToJavascript [Char] where toJavascript = fromLazyText . TL.pack
instance ToJavascript TS.Text where toJavascript = fromText
instance ToJavascript TL.Text where toJavascript = fromLazyText

javascriptSettings :: Q ShakespeareSettings
javascriptSettings = do
  toJExp <- [|toJavascript|]
  wrapExp <- [|Javascript|]
  unWrapExp <- [|unJavascript|]
  return $ defaultShakespeareSettings { toBuilder = toJExp
  , wrap = wrapExp
  , unwrap = unWrapExp
  }

js, julius :: QuasiQuoter
js = QuasiQuoter { quoteExp = \s -> do
    rs <- javascriptSettings
    quoteExp (shakespeare rs) s
    }

julius = js

jsFile, juliusFile :: FilePath -> Q Exp
jsFile fp = do
    rs <- javascriptSettings
    shakespeareFile rs fp

juliusFile = jsFile


jsFileReload, juliusFileReload :: FilePath -> Q Exp
jsFileReload fp = do
    rs <- javascriptSettings
    shakespeareFileDebug rs fp

juliusFileReload = jsFileReload

jsFileDebug, juliusFileDebug :: FilePath -> Q Exp
juliusFileDebug = jsFileReload
{-# DEPRECATED juliusFileDebug "Please use juliusFileReload instead." #-}
jsFileDebug = jsFileReload
{-# DEPRECATED jsFileDebug "Please use jsFileReload instead." #-}
