{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Text.Julius
    ( Julius
    , Javascript (..)
    , ToJavascript (..)
    , renderJulius
    , julius
    , juliusFile
    , juliusFileDebug
    ) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText, fromLazyText)
import Data.Monoid
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Text.Romeo
import Control.Monad (ap)

renderJavascript :: Javascript -> TL.Text
renderJavascript (Javascript b) = toLazyText b

renderJulius :: (url -> [(TS.Text, TS.Text)] -> TS.Text) -> Julius url -> TL.Text
renderJulius r s = renderJavascript $ s r

newtype Javascript = Javascript { unJavascript :: Builder }
    deriving Monoid
type Julius url = (url -> [(TS.Text, TS.Text)] -> TS.Text) -> Javascript

class ToJavascript a where
    toJavascript :: a -> Builder
instance ToJavascript [Char] where toJavascript = fromLazyText . TL.pack
instance ToJavascript TS.Text where toJavascript = fromText
instance ToJavascript TL.Text where toJavascript = fromLazyText

settings :: Q RomeoSettings
settings = RomeoSettings '#' '@' '^' `fmap` [|toJavascript|] `ap` [|Javascript|] `ap` [|unJavascript|]

julius :: QuasiQuoter
julius = QuasiQuoter { quoteExp = \s -> do
    rs <- settings
    quoteExp (romeo rs) s
    }

juliusFile :: FilePath -> Q Exp
juliusFile fp = do
    rs <- settings
    romeoFile rs fp

juliusFileDebug :: FilePath -> Q Exp
juliusFileDebug fp = do
    rs <- settings
    romeoFileDebug rs fp
