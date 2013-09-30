{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
-- | A Shakespearean module for Javascript templates, introducing type-safe,
-- compile-time variable and url interpolation.--
--
-- You might consider trying 'Text.Typescript' or 'Text.Coffee' which compile down to Javascript.
--
-- Further reading: <http://www.yesodweb.com/book/templates>
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
    , RawJavascript (..)

      -- * Typeclass for interpolated variables
    , ToJavascript (..)
    , RawJS (..)

      -- ** Rendering Functions
    , renderJavascript
    , renderJavascriptUrl

      -- ** internal, used by 'Text.Coffee'
    , javascriptSettings
      -- ** internal
    , juliusUsedIdentifiers
    ) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText, fromLazyText)
import Data.Monoid
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Text.Shakespeare
import Data.Aeson (Value)
import Data.Aeson.Encode (fromValue)

renderJavascript :: Javascript -> TL.Text
renderJavascript (Javascript b) = toLazyText b

-- | render with route interpolation. If using this module standalone, apart
-- from type-safe routes, a dummy renderer can be used:
-- 
-- > renderJavascriptUrl (\_ _ -> undefined) javascriptUrl
--
-- When using Yesod, a renderer is generated for you, which can be accessed
-- within the GHandler monad: 'Yesod.Core.Handler.getUrlRenderParams'.
renderJavascriptUrl :: (url -> [(TS.Text, TS.Text)] -> TS.Text) -> JavascriptUrl url -> TL.Text
renderJavascriptUrl r s = renderJavascript $ s r

-- | Newtype wrapper of 'Builder'.
newtype Javascript = Javascript { unJavascript :: Builder }
    deriving Monoid

-- | Return type of template-reading functions.
type JavascriptUrl url = (url -> [(TS.Text, TS.Text)] -> TS.Text) -> Javascript

asJavascriptUrl :: JavascriptUrl url -> JavascriptUrl url
asJavascriptUrl = id

-- | A typeclass for types that can be interpolated in CoffeeScript templates.
class ToJavascript a where
    toJavascript :: a -> Javascript
#if 0
instance ToJavascript [Char] where toJavascript = Javascript . fromLazyText . TL.pack
instance ToJavascript TS.Text where toJavascript = Javascript . fromText
instance ToJavascript TL.Text where toJavascript = Javascript . fromLazyText
instance ToJavascript Javascript where toJavascript = Javascript . unJavascript
instance ToJavascript Builder where toJavascript = Javascript
#endif
instance ToJavascript Bool where toJavascript = Javascript . fromText . TS.toLower . TS.pack . show
instance ToJavascript Value where toJavascript = Javascript . fromValue

newtype RawJavascript = RawJavascript Builder
instance ToJavascript RawJavascript where
    toJavascript (RawJavascript a) = Javascript a

class RawJS a where
    rawJS :: a -> RawJavascript

instance RawJS [Char] where rawJS = RawJavascript . fromLazyText . TL.pack
instance RawJS TS.Text where rawJS = RawJavascript . fromText
instance RawJS TL.Text where rawJS = RawJavascript . fromLazyText
instance RawJS Builder where rawJS = RawJavascript
instance RawJS Bool where rawJS = RawJavascript . unJavascript . toJavascript

javascriptSettings :: Q ShakespeareSettings
javascriptSettings = do
  toJExp <- [|toJavascript|]
  wrapExp <- [|Javascript|]
  unWrapExp <- [|unJavascript|]
  asJavascriptUrl' <- [|asJavascriptUrl|]
  return $ defaultShakespeareSettings { toBuilder = toJExp
  , wrap = wrapExp
  , unwrap = unWrapExp
  , modifyFinalValue = Just asJavascriptUrl'
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
    shakespeareFileReload rs fp

juliusFileReload = jsFileReload

jsFileDebug, juliusFileDebug :: FilePath -> Q Exp
juliusFileDebug = jsFileReload
{-# DEPRECATED juliusFileDebug "Please use juliusFileReload instead." #-}
jsFileDebug = jsFileReload
{-# DEPRECATED jsFileDebug "Please use jsFileReload instead." #-}

-- | Determine which identifiers are used by the given template, useful for
-- creating systems like yesod devel.
juliusUsedIdentifiers :: String -> [(Deref, VarType)]
juliusUsedIdentifiers = shakespeareUsedIdentifiers defaultShakespeareSettings
