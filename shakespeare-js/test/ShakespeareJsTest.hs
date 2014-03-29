{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module ShakespeareJsTest (specs) where

import Test.HUnit hiding (Test)
import Test.Hspec

import Prelude hiding (reverse)
#ifdef TEST_COFFEE
import Text.Coffee
#endif
import Text.Julius
import Quoter (quote, quoteFile, quoteFileReload)
import Data.List (intercalate)
import qualified Data.Text.Lazy as T
import qualified Data.List
import qualified Data.List as L
import Data.Text (Text, pack, unpack)
import Data.Monoid (mappend)
import Data.Aeson (toJSON)

join :: [String] -> String
#ifdef TEST_COFFEE
join l = (intercalate ";\n" l)
#else
join = intercalate "\n"
#endif

specs :: Spec
specs = describe "shakespeare-js" $ do
#if !(defined TEST_COFFEE || defined TEST_ROY || defined TEST_PURESCRIPT)
  it "julius" $ do
    let var = "x=2"
    let urlp = (Home, [(pack "p", pack "q")])
    flip jelper [quote|['שלום', @{Home}, #{rawJS var}, '@?{urlp}', ^{jmixin} ]|]
      $ intercalate " "
        [ "['שלום',"
        , "url, " ++ var ++ ","
        , "'url?p=q',"
        , "f(2) ]"
        ]


  it "juliusFile" $ do
    let var = "x=2"
    let urlp = (Home, [(pack "p", pack "q")])
    flip jelper $(quoteFile "test/juliuses/external1.julius") $ join
        [ "שלום"
        , var
        , "url"
        , "url?p=q"
        , "f(2)"
        ] ++ "\n"


  it "juliusFileReload" $ do
    let var = "x=2"
    let urlp = (Home, [(pack "p", pack "q")])
    flip jelper $(quoteFileReload "test/juliuses/external1.julius") $ join
        [ "שלום"
        , var
        , "url"
        , "url?p=q"
        , "f(2)"
        ] ++ "\n"
#endif

{- TODO
  it "juliusFileDebugChange" $ do
    let var = "somevar"
        test result = jelper result $(juliusFileDebug "test/juliuses/external2.julius")
    writeFile "test/juliuses/external2.julius" "var #{var} = 1;"
    test "var somevar = 1;"
    writeFile "test/juliuses/external2.julius" "var #{var} = 2;"
    test "var somevar = 2;"
    writeFile "test/juliuses/external2.julius" "var #{var} = 1;"
    -}


  it "julius module names" $
    let foo = "foo"
        double = 3.14 :: Double
        int = -5 :: Int
#ifdef TEST_COFFEE
    in jelper "var _this = this;\n\n(function(shakespeare_var_rawJSDataListreversefoo, shakespeare_var_rawJSLreversefoo, shakespeare_var_rawJSshowdouble, shakespeare_var_rawJSshowint) {\n  return [shakespeare_var_rawJSDataListreversefoo, shakespeare_var_rawJSLreversefoo, shakespeare_var_rawJSshowdouble, shakespeare_var_rawJSshowint];\n})(oof, oof, 3.14, -5);\n"
#else
#  ifdef TEST_ROY
    in jelper "(function(shakespeare_var_rawJSDataListreversefoo, shakespeare_var_rawJSLreversefoo, shakespeare_var_rawJSshowdouble, shakespeare_var_rawJSshowint) {\n    return [shakespeare_var_rawJSDataListreversefoo, shakespeare_var_rawJSLreversefoo, shakespeare_var_rawJSshowdouble, shakespeare_var_rawJSshowint];\n})(oof, oof, 3.14, -5);\n"
#  else
#    ifdef TEST_PURESCRIPT
    in jelper "var main = [ \"oof\", \"oof\", \"3.14\", \"-5\" ];"
#    else
    in jelper "[oof, oof, 3.14, -5]"
#    endif
#  endif
#endif

#ifdef TEST_PURESCRIPT
         [quote|var main = [#{rawJS $ Data.List.reverse foo}, #{rawJS $ L.reverse foo}, #{rawJS $ show double}, #{rawJS $ show int}]|]
#else
         [quote|[#{rawJS $ Data.List.reverse foo}, #{rawJS $ L.reverse foo}, #{rawJS $ show double}, #{rawJS $ show int}]|]
#endif

-- not valid coffeescript
#if !(defined TEST_COFFEE || defined TEST_ROY || defined TEST_PURESCRIPT)
  it "single dollar at and caret" $ do
    jelper "$@^" [quote|$@^|]
    jelper "#{@{^{" [quote|#\{@\{^\{|]
#endif

  it "dollar operator" $ do
    let val = (1 :: Int, (2 :: Int, 3 :: Int))
#if (defined TEST_COFFEE)
    jelper "var _this = this;\n\n(function(shakespeare_var_rawJSshowfstsndval) {\n  return shakespeare_var_rawJSshowfstsndval;\n})(2);\n" [quote|#{ rawJS $ show $ fst $ snd val }|]
    jelper "var _this = this;\n\n(function(shakespeare_var_rawJSshowfstsndval) {\n  return shakespeare_var_rawJSshowfstsndval;\n})(2);\n" [quote|#{ rawJS $ show $ fst $ snd val }|]
#else

#  if (defined TEST_ROY)
    jelper "(function(shakespeare_var_rawJSshowfstsndval) {\n    return shakespeare_var_rawJSshowfstsndval;\n})(2);\n" [quote|#{ rawJS $ show $ fst $ snd val }|]
    jelper "(function(shakespeare_var_rawJSshowfstsndval) {\n    return shakespeare_var_rawJSshowfstsndval;\n})(2);\n" [quote|#{ rawJS $ show $ fst $ snd val }|]

#  else
    jelper "2" [quote|#{ rawJS $ show $ fst $ snd val }|]
    jelper "2" [quote|#{ rawJS $ show $ fst $ snd $ val}|]
#  endif
#endif

#if (defined TEST_ROY)
  it "roy function wrapper" $ do
    let royInsert = rawJS "\"royInsert\""
    jelper "(function(shakespeare_var_royInsert) {\n    var roy = {\n        \"royInsert\": shakespeare_var_royInsert\n    };\n    return console.log(roy);\n})(\"royInsert\");\n" [quote|
let roy = { royInsert: #{royInsert} }
console.log roy
|]
#endif
  it "empty file" $ jelper "" [quote||]

  it "JSON data" $ jelper "\"Hello \\\"World!\\\"\"" [julius|#{toJSON "Hello \"World!\""}|]

  it "boolean interpolation" $ jelper
    "true false true false true false"
    [julius|#{True} #{False} #{toJSON True} #{toJSON False} #{rawJS True} #{rawJS False}|]

  it "^\\ should not be escaped" $ jelper
     "var re = /[^\\r]/;"
     [julius|var re = /[^\r]/;|]

  it "^\\{ should be escaped" $ jelper
     "var re = /[^{]/;"
     [julius|var re = /[^\{]/;|]


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




#ifndef TEST_ROY
jmixin :: JavascriptUrl u
jmixin = [quote|f(2)|]
#endif

jelper :: String -> JavascriptUrl Url -> Assertion
jelper res h = do
  T.pack res @=? renderJavascriptUrl render h

instance Show Url where
    show _ = "FIXME remove this instance show Url"
