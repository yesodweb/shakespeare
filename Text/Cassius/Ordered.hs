{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

-- | This module is the twin brother of module Text.Cassius.
-- The difference is that these parsers preserv the given order of attributes and mixin blocks.
--
-- > let bams = [cassiusMixin|
-- >               bam1:bam2
-- >               ^{bins}
-- >               bam3:bam4
-- >            |] :: Mixin
-- >     bins = [cassiusMixin|
-- >               bin1:bin2
-- >            |] :: Mixin
-- >  in renderCss ([Text.Ordered.lucius|foo{bar1:bar2;^{bams};bar3:bar4;}|] undefined)
-- > "foo{bar1:bar2;bam1:bam2;bin1:bin2;bam3:bam4;bar3:bar4}"

module Text.Cassius.Ordered
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
import Text.Lucius.Ordered qualified as Lucius.Ordered
import Text.Shakespeare (VarType)
import Text.Shakespeare.Base

-- | @since 2.0.30
cassius :: QuasiQuoter
cassius = QuasiQuoter { quoteExp = quoteExp Lucius.Ordered.lucius . i2b }

-- | @since 2.0.30
cassiusFile :: FilePath -> Q Exp
cassiusFile fp = do
    contents <- readFileRecompileQ fp
    quoteExp cassius contents

-- | @since 2.0.30
cassiusFileDebug :: FilePath -> Q Exp
cassiusFileDebug = cssFileDebug True [|parseTopLevels Ordered|] (parseTopLevels Ordered)

-- | @since 2.0.30
cassiusFileReload :: FilePath -> Q Exp
cassiusFileReload = cassiusFileDebug

-- | Determine which identifiers are used by the given template, useful for
-- creating systems like yesod devel.
-- | @since 2.0.30
cassiusUsedIdentifiers :: String -> [(Deref, VarType)]
cassiusUsedIdentifiers = cssUsedIdentifiers True (parseTopLevels Ordered)

-- | Create a mixin with Cassius syntax.
--
-- | @since 2.0.30
cassiusMixin :: QuasiQuoter
cassiusMixin = QuasiQuoter
    { quoteExp = quoteExp Lucius.Ordered.luciusMixin . i2bMixin
    }
