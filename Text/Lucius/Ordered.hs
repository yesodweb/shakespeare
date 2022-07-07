{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

-- | This module is the twin brother of module Text.Lucius.
-- The difference is that these parsers preserv the given order of attributes and mixin blocks.
--
-- > let bams = [Text.Ordered.cassiusMixin|
-- >               bam1:bam2
-- >               ^{bins}
-- >               bam3:bam4
-- >            |] :: Mixin
-- >     bins = [Text.Ordered.cassiusMixin|
-- >               bin1:bin2
-- >            |] :: Mixin
-- >  in renderCss ([lucius|foo{bar1:bar2;^{bams};bar3:bar4;}|] undefined)
-- > "foo{bar1:bar2;bam1:bam2;bin1:bin2;bam3:bam4;bar3:bar4}"

module Text.Lucius.Ordered
    ( -- * Parsing
      lucius
    , luciusFile
    , luciusFileDebug
    , luciusFileReload
      -- ** Mixins
    , luciusMixin
    , Mixin
      -- ** Runtime
    , luciusRT
    , luciusRT'
    , luciusRTMinified
      -- *** Mixin
    , luciusRTMixin
    , RTValue (..)
    , -- * Datatypes
      Css
    , CssUrl
      -- * Type class
    , ToCss (..)
      -- * Rendering
    , renderCss
    , renderCssUrl
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
    , parseTopLevels
    , luciusUsedIdentifiers
    ) where

import Text.Internal.CssCommon
import Text.Internal.Lucius
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Text.Internal.Css


-- |
-- >>> renderCss ([lucius|foo{bar:baz}|] undefined)
-- "foo{bar:baz}"
--
-- @since 2.0.30
lucius :: QuasiQuoter
lucius = luciusWithOrder Ordered

-- | @since 2.0.30
luciusFile :: FilePath -> Q Exp
luciusFile = luciusFileWithOrd Ordered

-- | @since 2.0.30
luciusFileDebug :: FilePath -> Q Exp
luciusFileDebug = luciusFileDebugWithOrder Ordered

-- | @since 2.0.30
luciusFileReload :: FilePath -> Q Exp
luciusFileReload = luciusFileDebug

-- | @since 2.0.30
luciusRT' :: TL.Text
          -> Either String ([(Text, Text)]
          -> Either String [TopLevel 'Resolved])
luciusRT' = luciusRTWithOrder' Ordered

-- | @since 2.0.30
luciusRT :: TL.Text -> [(Text, Text)] -> Either String TL.Text
luciusRT = luciusRTWithOrder Ordered

-- | @since 2.0.30
luciusRTMixin :: TL.Text -- ^ template
              -> Bool -- ^ minify?
              -> [(Text, RTValue)] -- ^ scope
              -> Either String TL.Text
luciusRTMixin = luciusRTMixinWithOrder Ordered

-- | @since 2.0.30
luciusRTMinified :: TL.Text -> [(Text, Text)] -> Either String TL.Text
luciusRTMinified = luciusRTMinifiedWithOrder Ordered

-- | @since 2.0.30
luciusMixin :: QuasiQuoter
luciusMixin = luciusMixinWithOrder Ordered
