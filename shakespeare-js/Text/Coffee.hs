{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
-- | A Shakespearean module for CoffeeScript, introducing type-safe,
-- compile-time variable and url interpolation. It is very similar to
-- "Text.Julius", save that the template is first compiled to Javascript with
-- the system tool @coffee@.
--
-- To use this module, @coffee@ must be installed on your system.
--
-- @#{...}@ is the Shakespearean standard for variable interpolation, but
-- CoffeeScript already uses that sequence for string interpolation. Therefore,
-- Shakespearean interpolation is introduced with @%{...}@.
--
-- Further reading:
--
-- 1. Shakespearean templates: <http://www.yesodweb.com/book/templates>
--
-- 2. CoffeeScript: <http://coffeescript.org/>
module Text.Coffee
    ( -- * Functions
      -- ** Template-Reading Functions
      -- | These QuasiQuoter and Template Haskell methods return values of
      -- type @'CoffeeUrl' url@. See the Yesod book for details.
      coffee
    , coffeeFile
    , coffeeFileReload
    , coffeeFileDebug
      -- ** Rendering Functions
    , renderCoffee
    , renderCoffeeUrl
      -- * Datatypes
    , Coffeescript
    , CoffeeUrl
      -- * Typeclass for interpolated variables
    , ToCoffee (..)

#ifdef TEST_EXPORT
    , coffeeSettings
#endif
    ) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText, fromLazyText)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Data.Monoid
import Text.Shakespeare

-- | render with route interpolation. If using this module standalone, apart
-- from type-safe routes, a dummy renderer can be used:
-- 
-- > renderCoffeeUrl (\_ _ -> undefined) coffeeUrl
--
-- When using Yesod, a renderer is generated for you, which can be accessed
-- within the GHandler monad: 'Yesod.Handler.getUrlRenderParams'.
renderCoffeeUrl
    :: (url -> [(TS.Text, TS.Text)] -> TS.Text) -- ^ Url renderer
    -> CoffeeUrl url -- ^ Value returned from template reader function
    -> TL.Text  -- ^ @CoffeeScript@ with variables and routes fully resolved
renderCoffeeUrl r s = renderCoffee $ s r

renderCoffee :: Coffeescript -> TL.Text
renderCoffee (Coffeescript c) = toLazyText c

-- | Newtype wrapper of 'Builder'.
newtype Coffeescript = Coffeescript { unCoffeescript :: Builder }
    deriving Monoid

-- | Return type of template-reading functions.
type CoffeeUrl url = (url -> [(TS.Text, TS.Text)] -> TS.Text) -> Coffeescript

-- | A typeclass for types that can be interpolated in CoffeeScript templates.
class ToCoffee c where
    toCoffee :: c -> Builder

instance ToCoffee [Char]  where toCoffee = fromLazyText . TL.pack

instance ToCoffee TS.Text where toCoffee = fromText
instance ToCoffee TL.Text where toCoffee = fromLazyText

coffeeSettings :: Q ShakespeareSettings
coffeeSettings = do
  toExp <- [|toCoffee|]
  wrapExp <- [|Coffeescript|]
  unWrapExp <- [|unCoffeescript|]
  return $ defaultShakespeareSettings { varChar = '%'
  , toBuilder = toExp
  , wrap = wrapExp
  , unwrap = unWrapExp
  , preConversion = Just PreConvert {
      preConvert = ReadProcess "coffee" ["-epb"]
    , preEscapeBegin = "`"
    -- ^ backticks means the Coffeescript compiler will pass-through to javascript.
    , preEscapeEnd = "`"
    , preEscapeIgnore = "'\""
    -- ^ backticks are ignored inside a quote
    }
  }

-- | Read inline, quasiquoted CoffeeScript.
coffee :: QuasiQuoter
coffee = QuasiQuoter { quoteExp = \s -> do
    rs <- coffeeSettings
    quoteExp (shakespeare rs) s
    }

-- | Read in a CoffeeScript template file. This function reads the file once, at
-- compile time.
coffeeFile :: FilePath -> Q Exp
coffeeFile fp = do
    rs <- coffeeSettings
    shakespeareFile rs fp

-- | Read in a CoffeeScript template file. This impure function uses
-- unsafePerformIO to re-read the file on every call, allowing for rapid
-- iteration.
coffeeFileReload :: FilePath -> Q Exp
coffeeFileReload fp = do
    rs <- coffeeSettings
    shakespeareFileDebug rs fp

-- | Deprecated synonym for 'coffeeFileReload'
coffeeFileDebug :: FilePath -> Q Exp
coffeeFileDebug = coffeeFileReload
{-# DEPRECATED coffeeFileDebug "Please use coffeeFileReload instead." #-}
