{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Text.Coffee
    ( ToCoffee (..)
    , Coffee
    , Coffeescript
    , coffee
    , coffeeFile
    , coffeeFileDebug
    , renderCoffee
    ) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText, fromLazyText)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import System.Process (readProcess)
import Data.Monoid
import Text.Shakespeare

renderCoffee :: (url -> [(TS.Text, TS.Text)] -> TS.Text) -> Coffee url -> IO TL.Text
renderCoffee r s = do
  out <- readProcess "coffee" ["-epb", TL.unpack $ toLazyText $ unCoffee $ s r] []
  return $ TL.pack out
  where unCoffee (Coffeescript c) = c

newtype Coffeescript = Coffeescript { unCoffeescript :: Builder }
    deriving Monoid

type Coffee url = (url -> [(TS.Text, TS.Text)] -> TS.Text) -> Coffeescript

-- the types that can be placed in a template
class ToCoffee c where
    toCoffee :: c -> Builder
instance ToCoffee [Char]  where toCoffee = fromLazyText . TL.pack
instance ToCoffee TS.Text where toCoffee = fromText
instance ToCoffee TL.Text where toCoffee = fromLazyText

settings :: Q ShakespeareSettings
settings = do
  toExp <- [|toCoffee|]
  wrapExp <- [|Coffeescript|]
  unWrapExp <- [|unCoffeescript|]
  return $ defaultShakespeareSettings { varChar = '%'
  , toBuilder = toExp
  , wrap = wrapExp
  , unwrap = unWrapExp
  }

coffee :: QuasiQuoter
coffee = QuasiQuoter { quoteExp = \s -> do
    rs <- settings
    quoteExp (shakespeare rs) s
    }

coffeeFile :: FilePath -> Q Exp
coffeeFile fp = do
    rs <- settings
    shakespeareFile rs fp

coffeeFileDebug :: FilePath -> Q Exp
coffeeFileDebug fp = do
    rs <- settings
    shakespeareFileDebug rs fp
