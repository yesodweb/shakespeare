{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
-- | A Shakespearean module for PureScript, introducing type-safe,
-- compile-time variable and url interpolation. It is exactly the same as
-- "Text.Roy", except that it uses @psc@ instead of @roy@.
--
-- To use this module, @psc@ must be installed on your system. You can
-- accomplish this by either running @cabal install purescript@, or specifying
-- it in the cabal file of your project.
--
-- If you interpolate variables,
-- the template is first wrapped with a function containing javascript variables representing shakespeare variables,
-- then compiled with @psc@,
-- and then the value of the variables are applied to the function.
-- This means that in production the template can be compiled
-- once at compile time and there will be no dependency in your production
-- system on @psc@.
--
-- Further reading:
--
-- 1. Shakespearean templates: <http://www.yesodweb.com/book/templates>
--
-- 2. PureScript: <https://github.com/paf31/purescript>
module Text.PureScript
    ( -- * Functions
      -- ** Template-Reading Functions
      -- | These QuasiQuoter and Template Haskell methods return values of
      -- type @'JavascriptUrl' url@. See the Yesod book for details.
      psc
    , pscFile
    , pscFileReload

#ifdef TEST_EXPORT
    , pscSettings
#endif
    ) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Text.Shakespeare
import Text.Julius

-- | The PureScript language compiles down to Javascript.
-- We do this compilation once at compile time to avoid needing to do it during the request.
-- We call this a preConversion because other shakespeare modules like Lucius use Haskell to compile during the request instead rather than a system call.
pscSettings :: Q ShakespeareSettings
pscSettings = do
  jsettings <- javascriptSettings
  return $ jsettings { varChar = '#'
  , preConversion = Just PreConvert {
      preConvert = ReadProcess "sh" ["-c", "TMP_IN=$(mktemp XXXXXXXXXX.ts); TMP_OUT=$(mktemp XXXXXXXXXX.js); cat /dev/stdin > ${TMP_IN} && /tmp/pscript/purescript/.cabal-sandbox/bin/psc --out ${TMP_OUT} ${TMP_IN} && cat ${TMP_OUT}; rm ${TMP_IN} && rm ${TMP_OUT}"]
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

-- | Read inline, quasiquoted PureScript.
psc :: QuasiQuoter
psc = QuasiQuoter { quoteExp = \s -> do
    ps <- pscSettings
    quoteExp (shakespeare ps) s
    }

-- | Read in a PureScript template file. This function reads the file once, at
-- compile time.
pscFile :: FilePath -> Q Exp
pscFile fp = do
    ps <- pscSettings
    shakespeareFile ps fp

-- | Read in a PureScript template file. This impure function uses
-- unsafePerformIO to re-read the file on every call, allowing for rapid
-- iteration.
pscFileReload :: FilePath -> Q Exp
pscFileReload fp = do
    ps <- pscSettings
    shakespeareFileReload ps fp
