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
    , cassiusOrd
    , cassiusFile
    , cassiusFileOrd
    , cassiusFileDebug
    , cassiusFileDebugOrd
    , cassiusFileReload
    , cassiusFileReloadOrd
      -- ** Mixims
    , cassiusMixin
    , cassiusMixinOrd
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
import Text.Lucius (lucius, luciusOrd)
import qualified Text.Lucius
import Text.IndentToBrace (i2b)

cassius :: QuasiQuoter
cassius = QuasiQuoter { quoteExp = quoteExp lucius . i2b }
{-# DEPRECATED cassius "Use 'cassiusOrd' instead" #-}

-- | Like 'cassius' but preserves the order of attributes and mixins
-- @since 2.0.30
cassiusOrd :: QuasiQuoter
cassiusOrd = QuasiQuoter { quoteExp = quoteExp luciusOrd . i2b }

cassiusFile :: FilePath -> Q Exp
cassiusFile = cassiusFileWithOrder Unordered
{-# DEPRECATED cassiusFile "Use 'cassiusFileOrd' instead" #-}

-- | Like 'cassiusFile' but preserves the order of attributes and mixins
-- @since 2.0.30
cassiusFileOrd :: FilePath -> Q Exp
cassiusFileOrd = cassiusFileWithOrder Ordered

cassiusFileWithOrder :: Order -> FilePath -> Q Exp
cassiusFileWithOrder order fp = do
  let qq = case order of
            Unordered -> cassius
            Ordered   -> cassiusOrd
  contents <- readFileRecompileQ fp
  quoteExp qq contents

cassiusFileDebug :: FilePath -> Q Exp
cassiusFileDebug = cassiusFileDebugWithOrder Unordered
{-# DEPRECATED cassiusFileDebug "Use 'cassiusFileDebugOrd' instead" #-}

cassiusFileReload :: FilePath -> Q Exp
cassiusFileReload = cassiusFileDebug
{-# DEPRECATED cassiusFileReload "Use 'cassiusFileReloadOrd' instead" #-}

-- | Like 'cassiusFileDebug' but preserves the order of attributes and mixins
-- @since 2.0.30
cassiusFileDebugOrd :: FilePath -> Q Exp
cassiusFileDebugOrd = cassiusFileDebugWithOrder Ordered

-- | Like 'cassiusFileReload' but preserves the order of attributes and mixins
-- @since 2.0.30
cassiusFileReloadOrd :: FilePath -> Q Exp
cassiusFileReloadOrd = cassiusFileDebugOrd

cassiusFileDebugWithOrder :: Order -> FilePath -> Q Exp
cassiusFileDebugWithOrder order =
  cssFileDebug True [|Text.Lucius.parseTopLevels order|] (Text.Lucius.parseTopLevels order)

-- | Determine which identifiers are used by the given template, useful for
-- creating systems like yesod devel.
cassiusUsedIdentifiers :: Order -> String -> [(Deref, VarType)]
cassiusUsedIdentifiers order =
  cssUsedIdentifiers True (Text.Lucius.parseTopLevels order)

-- | Create a mixin with Cassius syntax.
--
-- Since 2.0.3
cassiusMixin :: QuasiQuoter
cassiusMixin = QuasiQuoter
    { quoteExp = quoteExp Text.Lucius.luciusMixin . i2bMixin
    }
{-# DEPRECATED cassiusMixin "Use 'cassiusMixinOrd' instead" #-}

-- | Like 'cassiusMixin' but preserves the order of attributes and mixins
-- @since 2.0.30
cassiusMixinOrd :: QuasiQuoter
cassiusMixinOrd = QuasiQuoter
    { quoteExp = quoteExp Text.Lucius.luciusMixinOrd . i2bMixin
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
