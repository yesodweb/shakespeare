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

-- | The Roy language compiles down to Javascript.
-- We do this once at compile time to avoid needing to do it during the request.
-- We call this a preConversion because other shakespeare modules like Lucius use Haskell to compile during the request rather than a system call.
-- During the pre-conversion we first modify all Haskell insertions
-- so that they will be ignored by the Coffeescript compiler (backticks).
-- So %{var} is change to `%{var}` using the preEscapeBegin and preEscapeEnd.
-- preEscapeIgnore is used to not insert backtacks for variable already inside strings or backticks.
-- coffeescript will happily ignore the interpolations, and backticks would not be treated as escaping in that context.
coffeeSettings :: Q ShakespeareSettings
coffeeSettings = do
  jsettings <- javascriptSettings
  return $ jsettings { varChar = '%'
  , preConversion = Just PreConvert {
      preConvert = ReadProcess "coffee" ["-sp"]
    , preEscapeBegin = "`"
    , preEscapeEnd = "`"
    , preEscapeIgnoreBalanced = "'\"`"
    , preEscapeIgnoreLine = "#"
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
    shakespeareFileReload rs fp

-- | Deprecated synonym for 'coffeeFileReload'
coffeeFileDebug :: FilePath -> Q Exp
coffeeFileDebug = coffeeFileReload
{-# DEPRECATED coffeeFileDebug "Please use coffeeFileReload instead." #-}
