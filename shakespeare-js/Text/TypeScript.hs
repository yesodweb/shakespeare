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
-- The template is first compiled with tsc, and then wrapped with Yesod
-- variables. This means that in production the template can be compiled
-- once at compile time and there will be no dependency in your production
-- system on @tsc@. 
--
-- Your code:
--
-- > #{a} + 2
--
-- Result:
--
-- > ;(function(yesod_splice_a){
-- >   yesod_splice_a + 2
-- > })(#{a});
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
  return $ jsettings { varChar = '%'
  , preConversion = Just PreConvert {
      preConvert = ReadProcess "tsc" ["--nolib"]
    , preEscapeBegin = ""
    , preEscapeEnd = ""
    , preEscapeIgnoreBalanced = "'\""
    , preEscapeIgnoreLine = "//"
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
