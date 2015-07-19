{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
-- | A Shakespearean module for PureScript, introducing type-safe,
-- compile-time variable and url interpolation. It is simlary to 
-- "Text.Coffee" or "Text.TypeScript", except that it uses the purescript executable, @psc@
--
-- To use this module, @psc@ must be installed on your system. You can
-- accomplish this by either running @cabal install purescript@, or specifying
-- it in the cabal file of your project.
--
-- Unfortunately, variable interpolation is completely broken and assumes everything inserted is a String
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
      preConvert = ReadProcess "psc" ["--stdin", "--verbose-errors", "--no-prelude", "--no-prefix"]
    , preEscapeIgnoreBalanced = "'\""
    , preEscapeIgnoreLine = ""
    , wrapInsertion = Just WrapModule
      { wrapModuleImport = "foreign import"
      , wrapModuleMain = "module Shakespeare where"
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
