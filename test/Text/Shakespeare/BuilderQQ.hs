{-# LANGUAGE TemplateHaskellQuotes #-}
module Text.Shakespeare.BuilderQQ (builderQQ) where

import Prelude
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Exp(..))
import Text.Shakespeare
import Text.Shakespeare.Text (ToText(..))

builderQQ :: QuasiQuoter
builderQQ =
  shakespeare $
    defaultShakespeareSettings
      { justVarInterpolation = True
      , toBuilder = VarE 'toText
      , wrap = VarE 'id
      , unwrap = VarE 'id
      }
