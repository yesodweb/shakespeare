{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
-- | A Shakespearean module for general text processing, introducing type-safe,
-- compile-time variable interpolation.
--
-- Text templates use the same parser as for other shakespearean templates
-- which enables variable interpolation using @#{..}@.  The parser also
-- recognize the @@{..}@ and @^{..}@ syntax.
--
-- If it is necessary that your template produces the output containing one of
-- the interpolation syntax you can escape the sequence using a backslash:
--
-- > λ> :set -XQuasiQuotes
-- > λ> let bar = 23 :: Int in [st|#{bar}|] :: Text
--
-- produces "23", but
--
-- > λ> let bar = 23 :: Int in [st|#\{bar}|] :: Text
--
-- returns "#{bar}".  The escaping backslash is removed from the output.
--
-- Further reading:
-- Shakespearean templates: <https://www.yesodweb.com/book/shakespearean-templates>
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
    , sbt -- | strict text whose left edge is aligned with bar ('|')
    , lbt -- | lazy text, whose left edge is aligned with bar ('|')
    -- * Yesod code generation
    , codegen
    , codegenSt
    , codegenFile
    , codegenFileReload
    ) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText, fromLazyText)
import Data.Text.Lazy.Builder.Int (decimal)
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

instance ToText Int32 where toText = decimal
instance ToText Int64 where toText = decimal
instance ToText Int   where toText = decimal

settings :: Q ShakespeareSettings
settings = do
  toTExp <- [|toText|]
  wrapExp <- [|id|]
  unWrapExp <- [|id|]
  return $ defaultShakespeareSettings { toBuilder = toTExp
  , wrap = wrapExp
  , unwrap = unWrapExp
  }


stext, lt, st, text, lbt, sbt :: QuasiQuoter
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

dropBar :: [TL.Text] -> [TL.Text]
dropBar [] = []
dropBar (c:cx) = c:dropBar' cx
  where
    dropBar' txt = reverse $ drop 1 $ map (TL.drop 1 . TL.dropWhile (/= '|')) $ reverse txt

lbt = 
  QuasiQuoter { quoteExp = \s -> do
    rs <- settings
    render <- [|TL.unlines . dropBar . TL.lines . toLazyText|]
    rendered <- shakespeareFromString rs { justVarInterpolation = True } s
    return (render `AppE` rendered)
    }

sbt = 
  QuasiQuoter { quoteExp = \s -> do
    rs <- settings
    render <- [|TL.toStrict . TL.unlines . dropBar . TL.lines . toLazyText|]
    rendered <- shakespeareFromString rs { justVarInterpolation = True } s
    return (render `AppE` rendered)
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
