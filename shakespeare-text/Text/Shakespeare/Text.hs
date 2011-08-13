{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Text.Shakespeare.Text
    ( TextUrl
    , ToText (..)
    , renderTextUrl
    , stext
    , text
    , textFile
    , textFileDebug
    , st -- | strict text
    , lt -- | lazy text, same as stext :)
    ) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText, fromLazyText)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Text.Shakespeare

renderText :: Builder -> TL.Text
renderText = toLazyText

renderTextUrl :: RenderUrl url -> TextUrl url -> TL.Text
renderTextUrl r s = renderText $ s r

type TextUrl url = RenderUrl url -> Builder

class ToText a where
    toText :: a -> Builder
instance ToText [Char ] where toText = fromLazyText . TL.pack
instance ToText TS.Text where toText = fromText
instance ToText TL.Text where toText = fromLazyText

settings :: Q ShakespeareSettings
settings = do
  toTExp <- [|toText|]
  wrapExp <- [|id|]
  unWrapExp <- [|id|]
  return $ defaultShakespeareSettings { toBuilder = toTExp
  , wrap = wrapExp
  , unwrap = unWrapExp
  }

stext, lt, st, text :: QuasiQuoter
stext = 
  QuasiQuoter { quoteExp = \s -> do
    rs <- settings
    render <- [|renderText|]
    rendered <- shakespeareFromString rs { justVarInterpolation = True } s
    return (render `AppE` rendered)
    }
lt = stext

st = 
  QuasiQuoter { quoteExp = \s -> do
    rs <- settings
    render <- [|TL.toStrict . renderText|]
    rendered <- shakespeareFromString rs { justVarInterpolation = True } s
    return (render `AppE` rendered)
    }

text = QuasiQuoter { quoteExp = \s -> do
    rs <- settings
    quoteExp (shakespeare rs) s
    }

textFile :: FilePath -> Q Exp
textFile fp = do
    rs <- settings
    shakespeareFile rs fp


textFileDebug :: FilePath -> Q Exp
textFileDebug fp = do
    rs <- settings
    shakespeareFileDebug rs fp
