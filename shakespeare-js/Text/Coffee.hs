{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
-- | A Shakespearean module for CoffeeScript, introducing type-safe,
-- compile-time variable and url interpolation. It is exactly the same as
-- "Text.Julius", except that the template is first compiled to Javascript with
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
      -- type @'JavascriptUrl' url@. See the Yesod book for details.
      coffee
    , coffeeFile
    , coffeeFileReload
    , coffeeFileDebug

#ifdef TEST_EXPORT
    , coffeeSettings
#endif
    ) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Text.Shakespeare
import Text.Julius

coffeeSettings :: Q ShakespeareSettings
coffeeSettings = do
  jsettings <- javascriptSettings
  return $ jsettings { varChar = '%'
  , preConversion = Just PreConvert {
      preConvert = ReadProcess "coffee" ["-epb"]
    , preEscapeBegin = "`"
    -- ^ backticks means the Coffeescript compiler will pass-through to javascript.
    , preEscapeEnd = "`"
    , preEscapeIgnore = "'\"`"
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
