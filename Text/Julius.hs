{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
-- | A Shakespearean module for Javascript templates, introducing type-safe,
-- compile-time variable and url interpolation.--
--
-- You might consider trying 'Text.Typescript' or 'Text.Coffee' which compile down to Javascript.
--
-- Further reading: <http://www.yesodweb.com/book/shakespearean-templates>
module Text.Julius
    ( -- * Functions
      -- ** Template-Reading Functions
      -- | These QuasiQuoter and Template Haskell methods return values of
      -- type @'JavascriptUrl' url@. See the Yesod book for details.
      js
    , julius
    , juliusFile
    , jsFile
    , juliusFileDebug
    , jsFileDebug
    , juliusFileReload
    , jsFileReload

      -- * Datatypes
    , JavascriptUrl
    , Javascript (..)
    , RawJavascript (..)

      -- * Typeclass for interpolated variables
    , ToJavascript (..)
    , RawJS (..)

      -- ** Rendering Functions
    , renderJavascript
    , renderJavascriptUrl

      -- ** internal, used by 'Text.Coffee'
    , javascriptSettings
      -- ** internal
    , juliusUsedIdentifiers
    , asJavascriptUrl
    ) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText, fromLazyText)
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Text.Shakespeare
import Data.Aeson (Value, toJSON)
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as KeyMap
#endif
import Data.Aeson.Types (Value(..))
import Numeric (showHex)
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import Data.Text.Lazy.Builder (singleton, fromString)
import qualified Data.Text as T
import Data.Scientific (FPFormat(..), Scientific, base10Exponent)
import Data.Text.Lazy.Builder.Scientific (formatScientificBuilder)

renderJavascript :: Javascript -> TL.Text
renderJavascript (Javascript b) = toLazyText b

-- | render with route interpolation. If using this module standalone, apart
-- from type-safe routes, a dummy renderer can be used:
-- 
-- > renderJavascriptUrl (\_ _ -> undefined) javascriptUrl
--
-- When using Yesod, a renderer is generated for you, which can be accessed
-- within the GHandler monad: 'Yesod.Core.Handler.getUrlRenderParams'.
renderJavascriptUrl :: (url -> [(TS.Text, TS.Text)] -> TS.Text) -> JavascriptUrl url -> TL.Text
renderJavascriptUrl r s = renderJavascript $ s r

-- | Newtype wrapper of 'Builder'.
newtype Javascript = Javascript { unJavascript :: Builder }
    deriving (Semigroup, Monoid)

-- | Return type of template-reading functions.
type JavascriptUrl url = (url -> [(TS.Text, TS.Text)] -> TS.Text) -> Javascript

asJavascriptUrl :: JavascriptUrl url -> JavascriptUrl url
asJavascriptUrl = id

-- | A typeclass for types that can be interpolated in CoffeeScript templates.
class ToJavascript a where
    toJavascript :: a -> Javascript

instance ToJavascript Bool where toJavascript = Javascript . fromText . TS.toLower . TS.pack . show
instance ToJavascript Value where toJavascript = Javascript . encodeToTextBuilder
instance ToJavascript String where toJavascript = toJavascript . toJSON
instance ToJavascript TS.Text where toJavascript = toJavascript . toJSON
instance ToJavascript TL.Text where toJavascript = toJavascript . toJSON

-- | Encode a JSON 'Value' to a "Data.Text" 'Builder', which can be
-- embedded efficiently in a text-based protocol.
--
-- If you are going to immediately encode straight to a
-- 'L.ByteString', it is more efficient to use 'encodeToBuilder'
-- instead.
encodeToTextBuilder :: Value -> Builder
encodeToTextBuilder =
    go
  where
    go Null       = {-# SCC "go/Null" #-} "null"
    go (Bool b)   = {-# SCC "go/Bool" #-} if b then "true" else "false"
    go (Number s) = {-# SCC "go/Number" #-} fromScientific s
    go (String s) = {-# SCC "go/String" #-} string s
    go (Array v)
        | V.null v = {-# SCC "go/Array" #-} "[]"
        | otherwise = {-# SCC "go/Array" #-}
                      singleton '[' <>
                      go (V.unsafeHead v) <>
                      V.foldr f (singleton ']') (V.unsafeTail v)
      where f a z = singleton ',' <> go a <> z
    go (Object m) = {-# SCC "go/Object" #-}
        case fromObject m of
          (x:xs) -> singleton '{' <> one x <> foldr f (singleton '}') xs
          _      -> "{}"
      where f a z     = singleton ',' <> one a <> z
            one (k,v) = string k <> singleton ':' <> go v

#if MIN_VERSION_aeson(2,0,0)
    fromObject = H.toList . KeyMap.toHashMapText
#else
    fromObject = H.toList
#endif

string :: T.Text -> Builder
string s = {-# SCC "string" #-} singleton '"' <> quote s <> singleton '"'
  where
    quote q = case T.uncons t of
                Nothing      -> fromText h
                Just (!c,t') -> fromText h <> escape c <> quote t'
        where (h,t) = {-# SCC "break" #-} T.break isEscape q
    isEscape c = c == '\"' ||
                 c == '\'' ||
                 c == '\\' ||
                 c == '<'  ||
                 c == '>'  ||
                 c == '&'  ||
                 c < '\x20'
    escape '\"' = "\\\""
    escape '\'' = "\\\'"
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape '<' = "\\u003c"
    escape '>' = "\\u003e"
    escape '&' = "\\u0026"

    escape c
        | c < '\x20' = fromString $ "\\u" ++ replicate (4 - length h) '0' ++ h
        | otherwise  = singleton c
        where h = showHex (fromEnum c) ""

fromScientific :: Scientific -> Builder
fromScientific s = formatScientificBuilder format prec s
  where
    (format, prec)
      | base10Exponent s < 0 = (Generic, Nothing)
      | otherwise            = (Fixed,   Just 0)

newtype RawJavascript = RawJavascript Builder
instance ToJavascript RawJavascript where
    toJavascript (RawJavascript a) = Javascript a

class RawJS a where
    rawJS :: a -> RawJavascript

instance RawJS [Char] where rawJS = RawJavascript . fromLazyText . TL.pack
instance RawJS TS.Text where rawJS = RawJavascript . fromText
instance RawJS TL.Text where rawJS = RawJavascript . fromLazyText
instance RawJS Builder where rawJS = RawJavascript
instance RawJS Bool where rawJS = RawJavascript . unJavascript . toJavascript

javascriptSettings :: Q ShakespeareSettings
javascriptSettings = do
  toJExp <- [|toJavascript|]
  wrapExp <- [|Javascript|]
  unWrapExp <- [|unJavascript|]
  asJavascriptUrl' <- [|asJavascriptUrl|]
  return $ defaultShakespeareSettings { toBuilder = toJExp
  , wrap = wrapExp
  , unwrap = unWrapExp
  , modifyFinalValue = Just asJavascriptUrl'
  }

js, julius :: QuasiQuoter
js = QuasiQuoter { quoteExp = \s -> do
    rs <- javascriptSettings
    quoteExp (shakespeare rs) s
    }

julius = js

jsFile, juliusFile :: FilePath -> Q Exp
jsFile fp = do
    rs <- javascriptSettings
    shakespeareFile rs fp

juliusFile = jsFile


jsFileReload, juliusFileReload :: FilePath -> Q Exp
jsFileReload fp = do
    rs <- javascriptSettings
    shakespeareFileReload rs fp

juliusFileReload = jsFileReload

jsFileDebug, juliusFileDebug :: FilePath -> Q Exp
juliusFileDebug = jsFileReload
{-# DEPRECATED juliusFileDebug "Please use juliusFileReload instead." #-}
jsFileDebug = jsFileReload
{-# DEPRECATED jsFileDebug "Please use jsFileReload instead." #-}

-- | Determine which identifiers are used by the given template, useful for
-- creating systems like yesod devel.
juliusUsedIdentifiers :: String -> [(Deref, VarType)]
juliusUsedIdentifiers = shakespeareUsedIdentifiers defaultShakespeareSettings
