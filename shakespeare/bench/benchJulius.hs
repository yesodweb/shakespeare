-- | How long does it take for Julius to parse a file?
--
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Criterion.Main
import Text.Julius
import Numeric (showInt)
import qualified Data.ByteString.Lazy as L
import qualified Text.Blaze.Renderer.Utf8 as Utf8
import Data.Monoid (mconcat)
import Text.Blaze.Html5 (table, tr, td)

data Url = Home | Sub SubUrl
data SubUrl = SubUrl
showParams :: [(String, String)] -> String
showParams [] = ""
showParams _ = ""

render :: Url -> [(String, String)] -> String
render Home qs = "url" ++ showParams qs
render (Sub SubUrl) qs = "suburl" ++ showParams qs

main = defaultMain
    [ bench "Julius read" $ nf juliusRender [0,1]
    ]
  where
  juliusRender _ = renderJulius render $(juliusFile "tests/jquery-ui.julius")
