{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Text.Shakespeare.Text
    ( TextUrl
    , ToText (..)
    , renderTextUrl
    , text
    , textFile
    , textFileDebug
    ) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText, fromLazyText)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Text.Shakespeare

renderText :: Builder -> TL.Text
renderText = toLazyText

renderTextUrl :: Render url -> TextUrl url -> TL.Text
renderTextUrl r s = renderText $ s r

type QueryString = [(TS.Text, TS.Text)]
type Render url = (url -> QueryString -> TS.Text)
type TextUrl url = Render url -> Builder

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

text :: QuasiQuoter
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
