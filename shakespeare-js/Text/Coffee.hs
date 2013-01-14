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
-- If you interpolate variables,
-- the template is first wrapped with a function containing javascript variables representing shakespeare variables,
-- then compiled with @coffee@,
-- and then the value of the variables are applied to the function.
-- This means that in production the template can be compiled
-- once at compile time and there will be no dependency in your production
-- system on @coffee@. 
--
-- Your code:
--
-- >   b = 1
-- >   console.log(#{a} + b)
--
-- Function wrapper added to your coffeescript code:
--
-- > ((yesod_var_a) =>
-- >   b = 1
-- >   console.log(yesod_var_a + b)
-- > )
--
-- This is then compiled down to javascript, and the variables are applied:
--
-- > ;(function(yesod_var_a){
-- >   var b = 1;
-- >   console.log(yesod_var_a + b);
-- > })(#{a});
--
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
      preConvert = ReadProcess "coffee" ["-spb"]
    , preEscapeIgnoreBalanced = "'\"`"     -- don't insert backtacks for variable already inside strings or backticks.
    , preEscapeIgnoreLine = "#"            -- ignore commented lines
    , wrapInsertion = Just WrapInsertion { 
        wrapInsertionIndent = Just "  "
      , wrapInsertionStartBegin = "(("
      , wrapInsertionSeparator = ", "
      , wrapInsertionStartClose = ") =>"
      , wrapInsertionEnd = ")"
      , wrapInsertionApplyBegin = "("
      , wrapInsertionApplyClose = ")\n"
      }
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
