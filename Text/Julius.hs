{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances, NoMonomorphismRestriction, MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
module Text.Julius
    ( Julius
    , ToJavascript (..)
    , julius
    , juliusFile
    , juliusFileDebug
    , renderJulius
    ) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText, fromLazyText)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.JSON.Types as J
import qualified Text.JSON.Enumerator as JE
import Data.Text.Lazy.Encoding (decodeUtf8)
import Blaze.ByteString.Builder (toLazyByteString)
import Text.YesodTemplate

type Julius url = YesodEnv url -> Builder

renderJulius :: YesodEnv url -> Julius url -> TL.Text
renderJulius r s = toLazyText $ s r

class Lift j => ToJavascript j where
    toJavascript :: j -> Builder
instance ToJavascript [Char] where toJavascript = fromLazyText . TL.pack
instance ToJavascript TS.Text where toJavascript = fromText
instance ToJavascript TL.Text where toJavascript = fromLazyText
instance ToJavascript J.Root where
    toJavascript (J.RootObject o) = toJavascript $ J.ValueObject o
    toJavascript (J.RootArray o) = toJavascript $ J.ValueArray o
instance ToJavascript J.Value where
    toJavascript = fromLazyText . decodeUtf8 . toLazyByteString . JE.renderValue

instance Lift J.Root  where lift = lift . toLazyText . toJavascript
instance Lift J.Value where lift = lift . toLazyText . toJavascript

juliusTemplate :: YesodTemplate
juliusTemplate = defaultYesodTemplate {toBuilder = toJExp}
  where toJExp = [|toJavascript|]

julius :: QuasiQuoter
julius = QuasiQuoter { quoteExp = stringToTH juliusTemplate }

juliusFile :: FilePath -> Q Exp
juliusFile fp = file juliusTemplate fp

juliusFileDebug :: FilePath -> Q Exp
juliusFileDebug fp = fileDebug juliusTemplate fp
