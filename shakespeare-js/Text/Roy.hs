{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
-- | A Shakespearean module for Roy, introducing type-safe,
-- compile-time variable and url interpolation. It is exactly the same as
-- "Text.Julius", except that the template is first compiled to Javascript with
-- the system tool @roy@.
--
-- To use this module, @roy@ must be installed on your system.
--
-- If you interpolate variables,
-- the template is first wrapped with a function containing javascript variables representing shakespeare variables,
-- then compiled with @roy@,
-- and then the value of the variables are applied to the function.
-- This means that in production the template can be compiled
-- once at compile time and there will be no dependency in your production
-- system on @roy@. 
--
-- Your code:
--
-- > let b = 1
-- > console.log(#{a} + b)
--
-- Final Result:
--
-- > ;(function(shakespeare_var_a){
-- >   var b = 1;
-- >   console.log(shakespeare_var_a + b);
-- > })(#{a});
--
-- Further reading:
--
-- 1. Shakespearean templates: <http://www.yesodweb.com/book/templates>
--
-- 2. Roy: <http://http://roy.brianmckenna.org/>
module Text.Roy
    ( -- * Functions
      -- ** Template-Reading Functions
      -- | These QuasiQuoter and Template Haskell methods return values of
      -- type @'JavascriptUrl' url@. See the Yesod book for details.
      roy
    , royFile
    , royFileReload

#ifdef TEST_EXPORT
    , roySettings
#endif
    ) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Text.Shakespeare
import Text.Julius

-- | The Roy language compiles down to Javascript.
-- We do this compilation once at compile time to avoid needing to do it during the request.
-- We call this a preConversion because other shakespeare modules like Lucius use Haskell to compile during the request instead rather than a system call.
roySettings :: Q ShakespeareSettings
roySettings = do
  jsettings <- javascriptSettings
  return $ jsettings { varChar = '#'
  , preConversion = Just PreConvert {
      preConvert = ReadProcess "roy" ["--stdio", "--browser"]
    , preEscapeIgnoreBalanced = "'\""
    , preEscapeIgnoreLine = "//"
    , wrapInsertion = Just WrapInsertion {
        wrapInsertionIndent = Just "  "
      , wrapInsertionStartBegin = "(\\"
      , wrapInsertionSeparator = " "
      , wrapInsertionStartClose = " ->\n"
      , wrapInsertionEnd = ")"
      , wrapInsertionAddParens = True
      }
    }
  }

-- | Read inline, quasiquoted Roy.
roy :: QuasiQuoter
roy = QuasiQuoter { quoteExp = \s -> do
    rs <- roySettings
    quoteExp (shakespeare rs) s
    }

-- | Read in a Roy template file. This function reads the file once, at
-- compile time.
royFile :: FilePath -> Q Exp
royFile fp = do
    rs <- roySettings
    shakespeareFile rs fp

-- | Read in a Roy template file. This impure function uses
-- unsafePerformIO to re-read the file on every call, allowing for rapid
-- iteration.
royFileReload :: FilePath -> Q Exp
royFileReload fp = do
    rs <- roySettings
    shakespeareFileReload rs fp
