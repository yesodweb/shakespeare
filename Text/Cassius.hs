{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
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

import Text.Internal.Css
import Text.Shakespeare.Base
import Text.Shakespeare (VarType)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import qualified Data.Text.Lazy as TL
import Text.Internal.CssCommon
import Text.Lucius (lucius)
import qualified Text.Lucius
import Text.IndentToBrace (i2b)

cassius :: QuasiQuoter
cassius = QuasiQuoter { quoteExp = quoteExp lucius . i2b }

cassiusFile :: FilePath -> Q Exp
cassiusFile fp = do
    contents <- fmap TL.unpack $ qRunIO $ readUtf8File fp
    quoteExp cassius contents

cassiusFileDebug, cassiusFileReload :: FilePath -> Q Exp
cassiusFileDebug = cssFileDebug True [|Text.Lucius.parseTopLevels|] Text.Lucius.parseTopLevels
cassiusFileReload = cassiusFileDebug

-- | Determine which identifiers are used by the given template, useful for
-- creating systems like yesod devel.
cassiusUsedIdentifiers :: String -> [(Deref, VarType)]
cassiusUsedIdentifiers = cssUsedIdentifiers True Text.Lucius.parseTopLevels

-- | Create a mixin with Cassius syntax.
--
-- Since 2.0.3
cassiusMixin :: QuasiQuoter
cassiusMixin = QuasiQuoter
    { quoteExp = quoteExp Text.Lucius.luciusMixin . i2bMixin
    }

i2bMixin :: String -> String
i2bMixin s' =
    TL.unpack
        $ stripEnd "}"
        $ stripFront "mixin {"
        $ TL.strip
        $ TL.pack
        $ i2b
        $ unlines
        $ "mixin" : (map ("    " ++) $ lines s')
  where
    stripFront x y =
        case TL.stripPrefix x y of
            Nothing -> y
            Just z -> z
    stripEnd x y =
        case TL.stripSuffix x y of
            Nothing -> y
            Just z -> z
