{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
-- | A Shakespearean module for Markdown, introducing type-safe,
-- compile-time variable and url interpolation.
--
-- This module is similar to "Text.Coffee" in that the only "Shakespearean"
-- thing it does is variable interpolation. It does not have the
-- programmability of Hamlet, or the sorts of enhancements provided by
-- Lucius. It simply converts your Markdown into Html while interpolating
-- routes and variables.
--
-- This module uses the Markdown processing from "Text.Markdown".
--
-- Be aware that if you want to start a new line with a variable
-- interpolation, Markdown will read the "#" as a heading indicator. To
-- avoid this, put a backslash in front of the #.
module Text.Marcus
    ( -- * Functions
      -- ** Template-Reading Functions
      -- | These QuasiQuoter and Template Haskell methods return values of
      -- type @'HtmlUrl' url@. See the Yesod book for details.
      marcus
    , marcusFile
    , marcusFileReload

#ifdef TEST_EXPORT
    , marcusSettings
#endif
    ) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Text.Shakespeare

marcusSettings :: Q ShakespeareSettings
marcusSettings = do
  jsettings <- javascriptSettings
  return $ jsettings { varChar = '%'
  , preConversion = Just PreConvert {
      preConvert = ReadProcess "marcus" ["-spb"]
    , preEscapeIgnoreBalanced = "'\"`"     -- don't insert backtacks for variable already inside strings or backticks.
    , preEscapeIgnoreLine = "#"            -- ignore commented lines
    , wrapInsertion = Just WrapInsertion { 
        wrapInsertionIndent = Just "  "
      , wrapInsertionStartBegin = "("
      , wrapInsertionSeparator = ", "
      , wrapInsertionStartClose = ") =>"
      , wrapInsertionEnd = ""
      , wrapInsertionAddParens = False
      }
    }
  }

-- | Read inline, quasiquoted Markdown
marcus :: QuasiQuoter
marcus = QuasiQuoter { quoteExp = \s -> do
    rs <- marcusSettings
    quoteExp (shakespeare rs) s
    }

-- | Read in a Markdown template file. This function reads the file once, at
-- compile time.
marcusFile :: FilePath -> Q Exp
marcusFile fp = do
    rs <- marcusSettings
    shakespeareFile rs fp

-- | Read in a Markdown template file. This impure function uses
-- unsafePerformIO to re-read the file on every call, allowing for rapid
-- iteration.
marcusFileReload :: FilePath -> Q Exp
marcusFileReload fp = do
    rs <- marcusSettings
    shakespeareFileReload rs fp
