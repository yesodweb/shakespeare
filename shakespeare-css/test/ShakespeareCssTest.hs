{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module ShakespeareCssTest (specs) where

import Test.HUnit hiding (Test)
import Test.Hspec
import Test.Hspec.HUnit ()

import Prelude hiding (reverse)
import Text.Cassius
import Text.Lucius
import Data.List (intercalate)
import qualified Data.Text.Lazy as T
import qualified Data.Text as TS
import qualified Data.List
import qualified Data.List as L
import Data.Text (Text, pack, unpack)
import Data.Monoid (mappend)

specs :: [Spec]
specs = describe "shakespeare-css"
  [ it "cassius" caseCassius
  , it "cassiusFile" caseCassiusFile

  , it "cassiusFileDebug" $ do
    let var = "var"
    let selector = "foo"
    let urlp = (Home, [(pack "p", pack "q")])
    flip celper $(cassiusFileDebug "test/cassiuses/external1.cassius") $ concat
        [ "foo{background:#000;bar:baz;color:#F00}"
        , "bin{"
        , "background-image:url(url);"
        , "bar:bar;color:#7F6405;fvarx:someval;unicode-test:שלום;"
        , "urlp:url(url?p=q)}"
        ]

{- TODO
  , it "cassiusFileDebugChange" $ do
    let var = "var"
    writeFile "test/cassiuses/external2.cassius" "foo\n  #{var}: 1"
    celper "foo{var:1}" $(cassiusFileDebug "test/cassiuses/external2.cassius")
    writeFile "test/cassiuses/external2.cassius" "foo\n  #{var}: 2"
    celper "foo{var:2}" $(cassiusFileDebug "test/cassiuses/external2.cassius")
    writeFile "test/cassiuses/external2.cassius" "foo\n  #{var}: 1"
    -}


  , it "comments" $ do
    -- FIXME reconsider Hamlet comment syntax?
    celper "" [cassius|/* this is a comment */
/* another comment */
/*a third one*/|]


  , it "cassius pseudo-class" $
    flip celper [cassius|
a:visited
    color: blue
|] "a:visited{color:blue}"


  , it "ignores a blank line" $ do
    celper "foo{bar:baz}" [cassius|
foo

    bar: baz

|]


  , it "leading spaces" $
    celper "foo{bar:baz}" [cassius|
  foo
    bar: baz
|]


  , it "cassius all spaces" $
    celper "h1{color:green }" [cassius|
    h1
        color: green 
    |]


  , it "cassius whitespace and colons" $ do
    celper "h1:hover{color:green ;font-family:sans-serif}" [cassius|
    h1:hover
        color: green 
        font-family:sans-serif
    |]


  , it "cassius trailing comments" $
    celper "h1:hover {color:green ;font-family:sans-serif}" [cassius|
    h1:hover /* Please ignore this */
        color: green /* This is a comment. */
        /* Obviously this is ignored too. */
        font-family:sans-serif
    |]



  , it "cassius module names" $
    let foo = "foo"
        dub = 3.14::Double
        int = -5::Int
    in
      celper "sel{bar:oof oof 3.14 -5}"
        [cassius|
sel
    bar: #{Data.List.reverse foo} #{L.reverse foo} #{show dub} #{show int}
|]



  , it "single dollar at and caret" $ do
    celper "sel{att:$@^}" [cassius|
sel
    att: $@^
|]

    celper "sel{att:#{@{^{}" [cassius|
sel
    att: #\{@\{^{
|]


  , it "dollar operator" $ do
    let val = (1, (2, 3)) :: (Integer, (Integer, Integer))
    celper "sel{att:2}" [cassius|
sel
    att: #{ show $ fst $ snd val }
|]
    celper "sel{att:2}" [cassius|
sel
    att: #{ show $ fst $ snd $ val}
|]



  , it "embedded slash" $ do
    celper "sel{att:///}" [cassius|
sel
    att: ///
|]






  , it "multi cassius" $ do
    celper "foo{bar:baz;bar:bin}" [cassius|
foo
    bar: baz
    bar: bin
|]






  , it "lucius" $ do
    let var = "var"
    let urlp = (Home, [(pack "p", pack "q")])
    flip celper [lucius|
foo {
    background: #{colorBlack};
    bar: baz;
    color: #{colorRed};
}
bin {
        background-image: url(@{Home});
        bar: bar;
        color: #{(((Color 127) 100) 5)};
        f#{var}x: someval;
        unicode-test: שלום;
        urlp: url(@?{urlp});
}
|] $ concat
        [ "foo{background:#000;bar:baz;color:#F00}"
        , "bin{"
        , "background-image:url(url);"
        , "bar:bar;color:#7F6405;fvarx:someval;unicode-test:שלום;"
        , "urlp:url(url?p=q)}"
        ]



  , it "lucius file" $ do
      let var = "var"
      let urlp = (Home, [(pack "p", pack "q")])
      flip celper $(luciusFile "test/cassiuses/external1.lucius") $ concat
          [ "foo{background:#000;bar:baz;color:#F00}"
          , "bin{"
          , "background-image:url(url);"
          , "bar:bar;color:#7F6405;fvarx:someval;unicode-test:שלום;"
          , "urlp:url(url?p=q)}"
          ]

{-
  , it "lucius file debug" caseLuciusFileDebug
  -}




  , it "lucius nested" $ do
      celper "foo bar{baz:bin}" $(luciusFile "test/cassiuses/external-nested.lucius")
      celper "foo bar{baz:bin}" $(luciusFileDebug "test/cassiuses/external-nested.lucius")
      celper "foo bar{baz:bin}" [lucius|
        foo {
            bar {
                baz: bin;
            }
        }
        |]
      celper "foo1 bar,foo2 bar{baz:bin}" [lucius|
        foo1, foo2 {
            bar {
                baz: bin;
            }
        }
        |]


  , it "lucius charset" $ do
      celper (concat ["@charset \"utf-8\";"
        , "#content ul{list-style:none;padding:0 5em}"
        , "#content ul li{padding:1em 0}"
        , "#content ul li a{color:#419a56;font-family:'TeXGyreHerosBold',helvetica,arial,sans-serif;font-weight:bold;text-transform:uppercase;white-space:nowrap}"
        ]) [lucius|
@charset "utf-8";
#content ul
{
    list-style: none;
    padding: 0 5em;
    li
    {
        padding: 1em 0;
        a
        {
            color: #419a56;
            font-family: 'TeXGyreHerosBold',helvetica,arial,sans-serif;
            font-weight: bold;
            text-transform: uppercase;
            white-space: nowrap;
        }
    }
}
|]

  , it "lucius media" $ do
      celper "@media only screen{foo bar{baz:bin}}" $(luciusFile "test/cassiuses/external-media.lucius")
      celper "@media only screen{foo bar{baz:bin}}" $(luciusFileDebug "test/cassiuses/external-media.lucius")
      celper "@media only screen{foo bar{baz:bin}}" [lucius|
        @media only screen{
            foo {
                bar {
                    baz: bin;
                }
            }
        }
        |]


  , it "cassius removes whitespace" $ do
      celper "foo{bar:baz}" [cassius|
      foo
          bar     :    baz
      |]





  , it "lucius trailing comments" $
      celper "foo{bar:baz}" [lucius|foo{bar:baz;}/* ignored*/|]

  , it "lucius variables" $ celper "foo{bar:baz}" [lucius|
@myvar: baz;
foo {
    bar: #{myvar};
}
|]
  ,  it "lucius CDO/CDC tokens" $
       celper "*{a:b}" [lucius|
<!-- --> <!--
* {
  a: b;
}
--!>
|]
  , it "lucius @import statements" $
      celper "@import url(\"bla.css\");" [lucius|
@import "bla.css";
|]
  , it "lucius simple escapes" $
      celper "*{a:test}" [lucius|
* {
  a: t\65 st;
}
|]
  , it "lucius bounded escapes" $
      celper "*{a:teft}" [lucius|
* {
  a: t\000065ft;
}
|]
  , it "lucius case-insensitive keywords" $
       celper "@media foo{}" [lucius|
@MeDIa foo {
}
|]
  , it "lucius @page statements" $
       celper "@page:right{a:b;c:d}" [lucius|
@page :right {
a:b;
c:d;
}
|]
  , it "lucius runtime" $ Right (T.pack "foo{bar:baz}") @=? luciusRT (T.pack "foo { bar: #{myvar}}") [(TS.pack "myvar", TS.pack "baz")]
  , it "lucius runtime variables" $ Right (T.pack "foo{bar:baz}") @=? luciusRT (T.pack "@dummy: dummy; @myvar: baz; @dummy2: dummy; foo { bar: #{myvar}}") []
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



celper :: String -> CssUrl Url -> Assertion
celper res h = do
    let x = renderCssUrl render h
    T.pack res @=? x

caseCassius :: Assertion
caseCassius = do
    let var = "var"
    let urlp = (Home, [(pack "p", pack "q")])
    flip celper [cassius|
foo
    background: #{colorBlack}
    bar: baz
    color: #{colorRed}
bin
        background-image: url(@{Home})
        bar: bar
        color: #{(((Color 127) 100) 5)}
        f#{var}x: someval
        unicode-test: שלום
        urlp: url(@?{urlp})
|] $ concat
        [ "foo{background:#000;bar:baz;color:#F00}"
        , "bin{"
        , "background-image:url(url);"
        , "bar:bar;color:#7F6405;fvarx:someval;unicode-test:שלום;"
        , "urlp:url(url?p=q)}"
        ]

caseCassiusFile :: Assertion
caseCassiusFile = do
    let var = "var"
    let selector = "foo"
    let urlp = (Home, [(pack "p", pack "q")])
    flip celper $(cassiusFile "test/cassiuses/external1.cassius") $ concat
        [ "foo{background:#000;bar:baz;color:#F00}"
        , "bin{"
        , "background-image:url(url);"
        , "bar:bar;color:#7F6405;fvarx:someval;unicode-test:שלום;"
        , "urlp:url(url?p=q)}"
        ]

instance Show Url where
    show _ = "FIXME remove this instance show Url"


caseLuciusFileDebug :: Assertion
caseLuciusFileDebug = do
    let var = "var"
    writeFile "test/cassiuses/external2.lucius" "foo{#{var}: 1}"
    celper "foo{var:1}" $(luciusFileDebug "test/cassiuses/external2.lucius")
    writeFile "test/cassiuses/external2.lucius" "foo{#{var}: 2}"
    celper "foo{var:2}" $(luciusFileDebug "test/cassiuses/external2.lucius")
    writeFile "test/cassiuses/external2.lucius" "foo{#{var}: 1}"
