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
    , textFileReload
    , st -- | strict text
    , lt -- | lazy text, same as stext :)
    -- * Yesod code generation
    , codegen
    , codegenSt
    , codegenFile
    , codegenFileReload
    ) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText, fromLazyText)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Text.Shakespeare
import Data.Int (Int32, Int64)

renderTextUrl :: RenderUrl url -> TextUrl url -> TL.Text
renderTextUrl r s = toLazyText $ s r

type TextUrl url = RenderUrl url -> Builder

class ToText a where
    toText :: a -> Builder
instance ToText Builder where toText = id
instance ToText [Char ] where toText = fromLazyText . TL.pack
instance ToText TS.Text where toText = fromText
instance ToText TL.Text where toText = fromLazyText

instance ToText Int32 where toText = toText . show
instance ToText Int64 where toText = toText . show

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
    render <- [|toLazyText|]
    rendered <- shakespeareFromString rs { justVarInterpolation = True } s
    return (render `AppE` rendered)
    }
lt = stext

st = 
  QuasiQuoter { quoteExp = \s -> do
    rs <- settings
    render <- [|TL.toStrict . toLazyText|]
    rendered <- shakespeareFromString rs { justVarInterpolation = True } s
    return (render `AppE` rendered)
    }

text = QuasiQuoter { quoteExp = \s -> do
    rs <- settings
    quoteExp (shakespeare rs) $ filter (/='\r') s
    }


textFile :: FilePath -> Q Exp
textFile fp = do
    rs <- settings
    shakespeareFile rs fp


textFileDebug :: FilePath -> Q Exp
textFileDebug = textFileReload
{-# DEPRECATED textFileDebug "Please use textFileReload instead" #-}

textFileReload :: FilePath -> Q Exp
textFileReload fp = do
    rs <- settings
    shakespeareFileReload rs fp

-- | codegen is designed for generating Yesod code, including templates
-- So it uses different interpolation characters that won't clash with templates.
codegenSettings :: Q ShakespeareSettings
codegenSettings = do
  toTExp <- [|toText|]
  wrapExp <- [|id|]
  unWrapExp <- [|id|]
  return $ defaultShakespeareSettings { toBuilder = toTExp
  , wrap = wrapExp
  , unwrap = unWrapExp
  , varChar = '~'
  , urlChar = '*'
  , intChar = '&'
  , justVarInterpolation = True -- always!
  }

-- | codegen is designed for generating Yesod code, including templates
-- So it uses different interpolation characters that won't clash with templates.
-- You can use the normal text quasiquoters to generate code
codegen :: QuasiQuoter
codegen =
  QuasiQuoter { quoteExp = \s -> do
    rs <- codegenSettings
    render <- [|toLazyText|]
    rendered <- shakespeareFromString rs { justVarInterpolation = True } s
    return (render `AppE` rendered)
    }

-- | Generates strict Text
-- codegen is designed for generating Yesod code, including templates
-- So it uses different interpolation characters that won't clash with templates.
codegenSt :: QuasiQuoter
codegenSt =
  QuasiQuoter { quoteExp = \s -> do
    rs <- codegenSettings
    render <- [|TL.toStrict . toLazyText|]
    rendered <- shakespeareFromString rs { justVarInterpolation = True } s
    return (render `AppE` rendered)
    }

codegenFileReload :: FilePath -> Q Exp
codegenFileReload fp = do
    rs <- codegenSettings
    render <- [|TL.toStrict . toLazyText|]
    rendered <- shakespeareFileReload rs{ justVarInterpolation = True } fp
    return (render `AppE` rendered)

codegenFile :: FilePath -> Q Exp
codegenFile fp = do
    rs <- codegenSettings
    render <- [|TL.toStrict . toLazyText|]
    rendered <- shakespeareFile rs{ justVarInterpolation = True } fp
    return (render `AppE` rendered)
