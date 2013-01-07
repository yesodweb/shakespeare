{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
-- | A Shakespearean module for TypeScript, introducing type-safe,
-- compile-time variable and url interpolation. It is exactly the same as
-- "Text.Julius", except that the template is first compiled to Javascript with
-- the system tool @tsc@.
--
-- To use this module, @tsc@ must be installed on your system.
--
-- If you interpolate variables,
-- the template is first wrapped with a function containing javascript variables representing shakespeare variables,
-- then compiled with @tsc@,
-- and then the value of the variables are applied to the function.
-- This means that in production the template can be compiled
-- once at compile time and there will be no dependency in your production
-- system on @tsc@. 
--
-- Your code:
--
-- > var b = 1
-- > console.log(#{a} + b)
--
-- Final Result:
--
-- > ;(function(yesod_var_a){
-- >   var b = 1;
-- >   console.log(yesod_var_a + b);
-- > })(#{a});
--
--
-- Important Warnings! This integration is not ideal.
--
-- Due to the function wrapper, all type declarations must be in separate .d.ts files.
-- However, if you don't interpolate variables, no function wrapper will be
-- created, and you can make type declarations.
--
-- This does not work cross-platform!
--
-- Unfortunately tsc does not support stdin and stdout.
-- So a hack of writing to temporary files using the mktemp
-- command is used. This works on my version of Linux, but not for windows
-- unless perhaps you install a mktemp utility, which I have not tested.
-- Please vote up this bug: <http://typescript.codeplex.com/workitem/600>
--
-- Making this work on Windows would not be very difficult, it will just require a new
-- package with a dependency on a package like temporary.
--
-- Further reading:
--
-- 1. Shakespearean templates: <http://www.yesodweb.com/book/templates>
--
-- 2. TypeScript: <http://typescript.codeplex.com/>
module Text.TypeScript
    ( -- * Functions
      -- ** Template-Reading Functions
      -- | These QuasiQuoter and Template Haskell methods return values of
      -- type @'JavascriptUrl' url@. See the Yesod book for details.
      tsc
    , typeScriptFile
    , typeScriptFileReload

#ifdef TEST_EXPORT
    , typeScriptSettings
#endif
    ) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Text.Shakespeare
import Text.Julius

-- | The TypeScript language compiles down to Javascript.
-- We do this compilation once at compile time to avoid needing to do it during the request.
-- We call this a preConversion because other shakespeare modules like Lucius use Haskell to compile during the request instead rather than a system call.
typeScriptSettings :: Q ShakespeareSettings
typeScriptSettings = do
  jsettings <- javascriptSettings
  return $ jsettings { varChar = '#'
  , preConversion = Just PreConvert {
      preConvert = ReadProcess "sh" ["-c", "TMP_IN=$(mktemp XXXXXXXXXX.ts); TMP_OUT=$(mktemp XXXXXXXXXX.js); cat /dev/stdin > ${TMP_IN} && tsc --out ${TMP_OUT} ${TMP_IN} && cat ${TMP_OUT}; rm ${TMP_IN} && rm ${TMP_OUT}"]
    , preEscapeIgnoreBalanced = "'\""
    , preEscapeIgnoreLine = "//"
    , wrapInsertion = Just WrapInsertion { 
        wrapInsertionIndent = Nothing
      , wrapInsertionStartBegin = ";(function("
      , wrapInsertionSeparator = ", "
      , wrapInsertionStartClose = "){"
      , wrapInsertionEnd = "})"
      , wrapInsertionApplyBegin = "("
      , wrapInsertionApplyClose = ");\n"
      }
    }
  }

-- | Read inline, quasiquoted TypeScript
tsc :: QuasiQuoter
tsc = QuasiQuoter { quoteExp = \s -> do
    rs <- typeScriptSettings
    quoteExp (shakespeare rs) s
    }

-- | Read in a Roy template file. This function reads the file once, at
-- compile time.
typeScriptFile :: FilePath -> Q Exp
typeScriptFile fp = do
    rs <- typeScriptSettings
    shakespeareFile rs fp

-- | Read in a Roy template file. This impure function uses
-- unsafePerformIO to re-read the file on every call, allowing for rapid
-- iteration.
typeScriptFileReload :: FilePath -> Q Exp
typeScriptFileReload fp = do
    rs <- typeScriptSettings
    shakespeareFileReload rs fp
