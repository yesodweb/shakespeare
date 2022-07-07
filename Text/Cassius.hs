{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Text.Cassius
    ( -- * Datatypes
      Css
    , CssUrl
      -- * Type class
    , ToCss (..)
      -- * Rendering
    , renderCss
    , renderCssUrl
      -- * Parsing
    , cassius
    , cassiusFile
    , cassiusFileDebug
    , cassiusFileReload
      -- ** Mixims
    , cassiusMixin
    , Mixin
      -- * ToCss instances
      -- ** Color
    , Color (..)
    , colorRed
    , colorBlack
      -- ** Size
    , mkSize
    , AbsoluteUnit (..)
    , AbsoluteSize (..)
    , absoluteSize
    , EmSize (..)
    , ExSize (..)
    , PercentageSize (..)
    , percentageSize
    , PixelSize (..)
      -- * Internal
    , cassiusUsedIdentifiers
    ) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Text.IndentToBrace (i2b)
import Text.Internal.Cassius (i2bMixin)
import Text.Internal.Css
import Text.Internal.CssCommon
import Text.Internal.Lucius (parseTopLevels)
import qualified Text.Lucius as Lucius
import Text.Shakespeare (VarType)
import Text.Shakespeare.Base

cassius :: QuasiQuoter
cassius = QuasiQuoter { quoteExp = quoteExp Lucius.lucius . i2b }

cassiusFile :: FilePath -> Q Exp
cassiusFile fp = do
    contents <- readFileRecompileQ fp
    quoteExp cassius contents

cassiusFileDebug :: FilePath -> Q Exp
cassiusFileDebug = cssFileDebug True [|parseTopLevels Unordered|] (parseTopLevels Unordered)

cassiusFileReload :: FilePath -> Q Exp
cassiusFileReload = cassiusFileDebug

-- | Determine which identifiers are used by the given template, useful for
-- creating systems like yesod devel.
cassiusUsedIdentifiers :: String -> [(Deref, VarType)]
cassiusUsedIdentifiers = cssUsedIdentifiers True (parseTopLevels Unordered)

-- | Create a mixin with Cassius syntax.
--
-- Since 2.0.3
cassiusMixin :: QuasiQuoter
cassiusMixin = QuasiQuoter
    { quoteExp = quoteExp Lucius.luciusMixin . i2bMixin
    }
