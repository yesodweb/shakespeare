{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- | Module for parsing and rendering Hamlet templates at runtime, not compile
-- time. This uses the same Hamlet parsing as compile-time Hamlet, but has some
-- limitations, such as:
--
-- * No compile-time checking of validity
--
-- * Can't apply functions at runtime
--
-- * No URL rendering
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Text.Hamlet.Runtime
-- > import qualified Data.Map as Map
-- > import Text.Blaze.Html.Renderer.String (renderHtml)
-- >
-- > main :: IO ()
-- > main = do
-- >     template <- parseHamletTemplate defaultHamletSettings $ unlines
-- >         [ "<p>Hello, #{name}"
-- >         , "$if hungry"
-- >         , "  <p>Available food:"
-- >         , "  <ul>"
-- >         , "    $forall food <- foods"
-- >         , "      <li>#{food}"
-- >         ]
-- >     let hamletDataMap = Map.fromList
-- >             [ ("name", "Michael")
-- >             , ("hungry", toHamletData True) -- always True
-- >             , ("foods", toHamletData
-- >                 [ "Apples"
-- >                 , "Bananas"
-- >                 , "Carrots"
-- >                 ])
-- >             ]
-- >     html <- renderHamletTemplate template hamletDataMap
-- >     putStrLn $ renderHtml html
--
-- @since 2.0.6
module Text.Hamlet.Runtime
    ( HamletTemplate
    , HamletSettings
    , defaultHamletSettings
    , HamletData
    , ToHamletData (..)
    , parseHamletTemplate
    , readHamletTemplateFile
    , renderHamletTemplate
    ) where

import Control.Arrow ((***))
import Control.Monad.Catch (MonadThrow)
import Text.Hamlet (HamletSettings, defaultHamletSettings)
import qualified Text.Hamlet.RT as RT
import Data.Void (Void, absurd)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.String
import Text.Blaze.Html (Html, toHtml)
import Control.Monad (liftM)
import Control.Monad.IO.Class
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.ByteString as S
import qualified Data.Text as T

-- | A parsed Hamlet template. See 'parseHamletTemplate' and
-- 'readHamletTemplateFile'.
--
-- @since 2.0.6
newtype HamletTemplate = HamletTemplate RT.HamletRT

-- | A piece of data that can be embedded and passed to a Hamlet template (via
-- 'renderHamletTemplate').
--
-- This supplies an 'IsString' instance, so with @OverloadedStrings@ it will
-- support literal strings, which are converted to HTML via 'toHtml'. For other
-- datatypes, use 'toHamletData'.
--
-- @since 2.0.6
newtype HamletData = HamletData { unHamletData :: RT.HamletData Void }
instance IsString HamletData where
    fromString = HamletData . RT.HDHtml . fromString

-- | Data which can be passed to a Hamlet template.
--
-- @since 2.0.6
class ToHamletData a where
    toHamletData :: a -> HamletData
instance ToHamletData HamletData where
    toHamletData = id
instance a ~ HamletData => ToHamletData [a] where
    toHamletData = HamletData . RT.HDList . map (\x -> [([], unHamletData x)])
instance a ~ HamletData => ToHamletData (Maybe a) where
    toHamletData = HamletData . RT.HDMaybe . fmap (\x -> [([], unHamletData x)])
instance ToHamletData Text where
    toHamletData = toHamletData . toHtml
instance ToHamletData Html where
    toHamletData = HamletData . RT.HDHtml
instance ToHamletData Bool where
    toHamletData = HamletData . RT.HDBool

-- | Parse an in-memory Hamlet template. This operation may fail if the
-- template is not parsable.
--
-- @since 2.0.6
parseHamletTemplate :: MonadThrow m => HamletSettings -> String -> m HamletTemplate
parseHamletTemplate set str = HamletTemplate `liftM` RT.parseHamletRT set str

-- | Same as 'parseHamletTemplate', but reads from a file. The file is assumed
-- to be UTF-8 encoded (same assumption as compile-time Hamlet).
--
-- @since 2.0.6
readHamletTemplateFile :: (MonadThrow m, MonadIO m) => HamletSettings -> FilePath -> m HamletTemplate
readHamletTemplateFile set fp = do
    bs <- liftIO $ S.readFile fp
    parseHamletTemplate set $ T.unpack $ decodeUtf8With lenientDecode bs

-- | Render a runtime Hamlet template, together with a 'Map' of variables to
-- pass in, into an 'Html' value. This can fail if the template references a
-- variable that is not present in the @Map@.
--
-- @since 2.0.6
renderHamletTemplate :: MonadThrow m => HamletTemplate -> Map Text HamletData -> m Html
renderHamletTemplate (HamletTemplate rt) m =
    RT.renderHamletRT' True rt m' renderUrl
  where
    m' = map (return . T.unpack *** unHamletData) $ Map.toList m
    renderUrl url _ = absurd url
