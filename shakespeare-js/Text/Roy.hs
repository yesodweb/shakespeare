{-# LANGUAGE TemplateHaskell #-}
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
-- @#{...}@ is the Shakespearean standard for variable interpolation, but
-- CoffeeScript already uses that sequence for string interpolation.
-- Therefore, it seems more future-proof to use @%{...}@ for interpolation
--
-- Integration with Roy is a bit rough right now.
-- You can only perorm a shakespeare insertion inside a Roy string.
-- This should work well for urls and strings.
-- Otherwise you should stick your Haskell into Julius as a window variable,
-- and then retrieve it in your Roy code.
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
-- We do this once at compile time to avoid needing to do it during the request.
-- We call this a preConversion because other shakespeare modules like Lucius use Haskell to compile during the request rather than a system call.
roySettings :: Q ShakespeareSettings
roySettings = do
  jsettings <- javascriptSettings
  return $ jsettings { varChar = '%'
  , preConversion = Just PreConvert {
      preConvert = ReadProcess "roy" ["--stdio"]
    , preEscapeBegin = "`"
    , preEscapeEnd = "`"
    , preEscapeIgnore = "'\"`"
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
    shakespeareFileDebug rs fp
