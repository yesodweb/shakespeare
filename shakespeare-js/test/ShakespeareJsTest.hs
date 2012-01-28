{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module ShakespeareJsTest (specs) where

import Test.HUnit hiding (Test)
import Test.Hspec
import Test.Hspec.HUnit ()

import Prelude hiding (reverse)
#ifdef TEST_COFFEE
import Text.Coffee
#else
import Text.Julius
#endif
import Quoter (quote, quoteFile, quoteFileReload)
import Data.List (intercalate)
import qualified Data.Text.Lazy as T
import qualified Data.List
import qualified Data.List as L
import Data.Text (Text, pack, unpack)
import Data.Monoid (mappend)

specs :: [Spec]
specs = describe "shakespeare-js"
  [ it "julius" $ do
    let var = "x=2"
    let urlp = (Home, [(pack "p", pack "q")])
    flip jelper [quote|שלום
@{Home} #{var}
@?{urlp}
^{jmixin}
|] $ intercalate "\n"
        [ "שלום"
        , "url " ++ var
        , "url?p=q"
        , "x=2;"
        ] ++ "\n"


  , it "juliusFile" $ do
    let var = "x=2"
    let urlp = (Home, [(pack "p", pack "q")])
    flip jelper $(quoteFile "test/juliuses/external1.julius") $ unlines
        [ "שלום"
        , var
        , "url"
        , "url?p=q"
        , "x=2;"
        ]


  , it "juliusFileDebug" $ do
    let var = "x=2"
    let urlp = (Home, [(pack "p", pack "q")])
    flip jelper $(quoteFileReload "test/juliuses/external1.julius") $ unlines
        [ "שלום"
        , var
        , "url"
        , "url?p=q"
        , "x=2;"
        ]

{- TODO
  , it "juliusFileDebugChange" $ do
      let var = "somevar"
          test result = jelper result $(juliusFileDebug "test/juliuses/external2.julius")
      writeFile "test/juliuses/external2.julius" "var #{var} = 1;"
      test "var somevar = 1;"
      writeFile "test/juliuses/external2.julius" "var #{var} = 2;"
      test "var somevar = 2;"
      writeFile "test/juliuses/external2.julius" "var #{var} = 1;"
      -}


  , it "julius module names" $
    let foo = "foo"
        double = 3.14 :: Double
        int = -5 :: Int in
      jelper "oof oof 3.14 -5"
        [quote|#{Data.List.reverse foo} #{L.reverse foo} #{show double} #{show int}|]


  , it "single dollar at and caret" $ do
    jelper "$@^" [quote|$@^|]
    jelper "#{@{^{" [quote|#\{@\{^\{|]


  , it "dollar operator" $ do
    let val = (1 :: Int, (2 :: Int, 3 :: Int))
    jelper "2" [quote|#{ show $ fst $ snd val }|]
    jelper "2" [quote|#{ show $ fst $ snd $ val}|]
  ]



data Url = Home | Sub SubUrl
data SubUrl = SubUrl
render :: Url -> [(Text, Text)] -> Text
render Home qs = pack "url" `mappend` showParams qs
render (Sub SubUrl) qs = pack "suburl" `mappend` showParams qs

showParams :: [(Text, Text)] -> Text
showParams [] = pack ""
showParams z =
    pack $ '?' : intercalate "&" (map go z)
  where
    go (x, y) = go' x ++ '=' : go' y
    go' = concatMap encodeUrlChar . unpack

-- | Taken straight from web-encodings; reimplemented here to avoid extra
-- dependencies.
encodeUrlChar :: Char -> String
encodeUrlChar c
    -- List of unreserved characters per RFC 3986
    -- Gleaned from http://en.wikipedia.org/wiki/Percent-encoding
    | 'A' <= c && c <= 'Z' = [c]
    | 'a' <= c && c <= 'z' = [c]
    | '0' <= c && c <= '9' = [c]
encodeUrlChar c@'-' = [c]
encodeUrlChar c@'_' = [c]
encodeUrlChar c@'.' = [c]
encodeUrlChar c@'~' = [c]
encodeUrlChar ' ' = "+"
encodeUrlChar y =
    let (a, c) = fromEnum y `divMod` 16
        b = a `mod` 16
        showHex' x
            | x < 10 = toEnum $ x + (fromEnum '0')
            | x < 16 = toEnum $ x - 10 + (fromEnum 'A')
            | otherwise = error $ "Invalid argument to showHex: " ++ show x
     in ['%', showHex' b, showHex' c]




#ifdef TEST_COFFEE
jmixin :: CoffeeUrl u
#else
jmixin :: JavascriptUrl u
#endif
jmixin = [quote|x=2;|]

#ifdef TEST_COFFEE
jelper :: String -> CoffeeUrl Url -> Assertion
jelper res h = do
  T.pack res @=? renderCoffeeUrl render h
#else
jelper :: String -> JavascriptUrl Url -> Assertion
jelper res h = do
  T.pack res @=? renderJavascriptUrl render h
#endif

instance Show Url where
    show _ = "FIXME remove this instance show Url"
