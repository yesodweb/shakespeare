{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.MarcusSpec (spec) where

import Data.Text.Lazy.Builder
import Test.Hspec
import Data.Text (Text)
import qualified Data.Text.Lazy as LT

import TestUrl
import Text.Marcus

-- | Helper. Or should I say melper?
shouldResult :: ((Url -> [(Text, Text)] -> Text) -> Builder)
             -> LT.Text
             -> Expectation
shouldResult h res = toLazyText (h renderUrl) `shouldBe` res

-- Main

spec :: Spec
spec = do
  let (x,y,z) = (3 :: Int, "Foo" :: Text, "Bar" :: String)
  describe "marcus" $ do
    it "basic" $ [marcus|Hello world|] `shouldResult` "<p>Hello world</p>"
    it "var interpolation" $
      [marcus|\#{x} #{y} #{z}|] `shouldResult` "<p>3 Foo Bar</p>"
    it "heading1" $ [marcus|
Heading
=======
      |] `shouldResult` "<h1>Heading</h1>"
    it "heading2" $ [marcus|
Heading
-------
      |] `shouldResult` "<h2>Heading</h2>"
    it "heading3" $ [marcus|### Heading|] `shouldResult` "<h3>Heading</h3>"
    it "heading with var" $ [marcus|#\#{y}|] `shouldResult` "<h1>Foo</h1>"
    it "routes" $ [marcus|* @{Home}|] `shouldResult` "<ul><li>url</li></ul>"
    it "Larger document" $ [marcus|
# Just a big test document for the hell of it

Here I write some lorem ipsum dolores.

* Lorem
* Ipsum

1. Rutabaga
4. *Turnip*
1. **Squash**
      |] `shouldResult` "<h1>Just a big test document for the hell of it</h1><p>Here I write some lorem ipsum dolores.</p><ul><li>Lorem</li><li>Ipsum</li></ul><ol><li>Rutabaga</li><li><i>Turnip</i></li><li><b>Squash</b></li></ol>"
