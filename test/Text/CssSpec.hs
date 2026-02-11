{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O0 #-}
module Text.CssSpec (spec) where

import Test.HUnit hiding (Test)
import Test.Hspec

import Prelude hiding (reverse)
import Text.Cassius
import qualified Text.Cassius.Ordered as Ordered
import Text.Lucius
import qualified Text.Lucius.Ordered as Ordered
import Data.List (intercalate)
import qualified Data.Text.Lazy as T
import qualified Data.Text as TS
import qualified Data.List
import qualified Data.List as L
import Data.Text (Text, pack, unpack)

spec :: Spec
spec = do
  describe "Unordered parsers" $ do
    it "cassius" caseCassius
    it "cassiusFile" caseCassiusFile
    it "cassius single comment" caseCassiusSingleComment
    it "cassius leading comment" caseCassiusLeadingComment

    it "cassiusFileDebug" $ do
      let var = "var"
      let selector = "foo"
      let urlp = (Home, [(pack "p", pack "q")])
      flip celper $(cassiusFileDebug "test/cassiuses/external1.cassius") $ concat
          [ "foo {\n    background: #000;\n    bar: baz;\n    color: #F00;\n}\n"
          , "bin {\n"
          , "    background-image: url(url);\n"
          , "    bar: bar;\n    color: #7F6405;\n    fvarx: someval;\n    unicode-test: שלום;\n"
          , "    urlp: url(url?p=q);\n}\n"
          ]

{- TODO
    it "cassiusFileDebugChange" $ do
    let var = "var"
    writeFile "test/cassiuses/external2.cassius" "foo\n  #{var}: 1"
    celper "foo{var:1}" $(cassiusFileDebug "test/cassiuses/external2.cassius")
    writeFile "test/cassiuses/external2.cassius" "foo\n  #{var}: 2"
    celper "foo{var:2}" $(cassiusFileDebug "test/cassiuses/external2.cassius")
    writeFile "test/cassiuses/external2.cassius" "foo\n  #{var}: 1"
    -}


    it "cassius comments" $ do
      -- FIXME reconsider Hamlet comment syntax?
      celper "" [cassius|/* this is a comment */
/* another comment */
/*a third one*/|]

    it "cassius pseudo-class" $
      flip celper [cassius|
a:visited
    color: blue
|] "a:visited{color:blue}"


    it "ignores a blank line" $ do
      celper "foo{bar:baz}" [cassius|
foo

    bar: baz

|]


    it "leading spaces" $
      celper "foo{bar:baz}" [cassius|
  foo
    bar: baz
|]


    it "cassius all spaces" $
      celper "h1{color:green }" [cassius|
    h1
        color: green 
    |]


    it "cassius whitespace and colons" $ do
      celper "h1:hover{color:green ;font-family:sans-serif}" [cassius|
    h1:hover
        color: green 
        font-family:sans-serif
    |]


    it "cassius trailing comments" $
      celper "h1:hover{color:green ;font-family:sans-serif}" [cassius|
    h1:hover /* Please ignore this */
        color: green /* This is a comment. */
        /* Obviously this is ignored too. */
        font-family:sans-serif
    |]

    it "cassius nesting" $
      celper "foo bar{baz:bin}" [cassius|
    foo
        bar
            baz: bin
    |]

    it "cassius variable" $
      celper "foo bar{baz:bin}" [cassius|
    @binvar: bin
    foo
        bar
            baz: #{binvar}
    |]

    it "cassius trailing semicolon" $
      celper "foo bar{baz:bin}" [cassius|
    @binvar: bin
    foo
        bar
            baz: #{binvar};
    |]



    it "cassius module names" $ do
      let foo = "foo"
          dub = 3.14::Double
          int = -5::Int
      celper "sel{bar:oof oof 3.14 -5}"
        [cassius|
sel
    bar: #{Data.List.reverse foo} #{L.reverse foo} #{show dub} #{show int}
|]



    it "single dollar at and caret" $ do
      celper "sel{att:$@^}" [cassius|
sel
    att: $@^
|]

  {-
      celper "sel{att:#{@{^{}" [cassius|
sel
    att: #\{@\{^{
|]
-}


    it "dollar operator" $ do
      let val = (1, (2, 3)) :: (Integer, (Integer, Integer))
      celper "sel{att:2}" [cassius|
sel
    att: #{ show $ fst $ snd val }
|]
      celper "sel{att:2}" [cassius|
sel
    att: #{ show $ fst $ snd $ val}
|]



    it "embedded slash" $ do
      celper "sel{att:///}" [cassius|
sel
    att: ///
|]






    it "multi cassius" $ do
      celper "foo{bar:baz;bar:bin}" [cassius|
foo
    bar: baz
    bar: bin
|]






    it "lucius" $ do
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



    it "lucius file" $ do
      let var = "var"
      let urlp = (Home, [(pack "p", pack "q")])
      flip celper $(luciusFile "test/cassiuses/external1.lucius") $ concat
          [ "foo{background:#000;bar:baz;color:#F00}"
          , "bin{"
          , "background-image:url(url);"
          , "bar:bar;color:#7F6405;fvarx:someval;unicode-test:שלום;"
          , "urlp:url(url?p=q)}"
          ]

    it "lucius file debug" caseLuciusFileDebug




    it "lucius nested" $ do
      celper "foo bar{baz:bin}" $(luciusFile "test/cassiuses/external-nested.lucius")
      celper "foo bar {\n    baz: bin;\n}\n" $(luciusFileDebug "test/cassiuses/external-nested.lucius")
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


    it "lucius charset" $ do
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

    it "lucius media" $ do
      celper "@media only screen{foo bar{baz:bin}}" $(luciusFile "test/cassiuses/external-media.lucius")
      celper "@media only screen {\n    foo bar {\n        baz: bin;\n    }\n}\n" $(luciusFileDebug "test/cassiuses/external-media.lucius")
      celper "@media only screen{foo bar{baz:bin}}" [lucius|
        @media only screen{
            foo {
                bar {
                    baz: bin;
                }
            }
        }
        |]

    it "lucius supports" $ do
      celper "@supports only screen{hana dul{set:net}}" $(luciusFile "test/cassiuses/external-supports.lucius")
      celper "@supports only screen {\n    hana dul {\n        set: net;\n    }\n}\n" $(luciusFileDebug "test/cassiuses/external-supports.lucius")
      celper "@supports only screen    {hana,dul{set:net;dasut:yeosut}}" [lucius|
        @supports only screen    {
            hana, dul {
                set: net;
                dasut: yeosut;
            }
        }
        |]

    it "lucius @layer block" $
      celper "@layer utilities{.padding{padding:1rem}}" [lucius|
        @layer utilities{
            .padding { padding: 1rem; }
        }
        |]

    it "lucius @layer declaration" $
      celper "@layer utilities;" [lucius|
        @layer utilities;
        |]

    it "lucius @layer multiple" $
      celper "@layer utilities, components;" [lucius|
        @layer utilities, components;
        |]

    it "lucius @layer anonymous" $
      celper "@layer {.padding{padding:1rem}}" [lucius|
        @layer {
            .padding { padding: 1rem; }
        }
        |]

    it "lucius @media inside @layer" $
      celper "@layer components {@media only screen {.btn{font-size:1.5rem}}}" [lucius|
        @layer components {
            @media only screen {
                .btn { font-size: 1.5rem; }
            }
        }
        |]

    it "lucius @layer inside @media" $
      celper "@media (max-width: 600px) {@layer utilities {.hidden{display:none}}}" [lucius|
        @media (max-width: 600px) {
            @layer utilities {
                .hidden { display: none; }
            }
        }
        |]

    it "lucius @supports inside @layer" $
      celper "@layer base {@supports (display: grid) {.grid{display:grid}}}" [lucius|
        @layer base {
            @supports (display: grid) {
                .grid { display: grid; }
            }
        }
        |]

    it "lucius @media inside @media" $
      celper "@media screen {@media (min-width: 768px) {.container{max-width:720px}}}" [lucius|
        @media screen {
            @media (min-width: 768px) {
                .container { max-width: 720px; }
            }
        }
        |]

    {-
    it "cassius removes whitespace" $ do
      celper "foo{bar:baz}" [cassius|
      foo
          bar     :    baz
      |]
      -}





    it "lucius trailing comments" $
      celper "foo{bar:baz}" [lucius|foo{bar:baz;}/* ignored*/|]

    it "lucius variables" $ celper "foo{bar:baz}" [lucius|
@myvar: baz;
foo {
    bar: #{myvar};
}
|]
    it "lucius CDO/CDC tokens" $
       celper "*{a:b}" [lucius|
<!-- --> <!--
* {
  a: b;
}
-->
|]
    it "lucius @import statements" $
      celper "@import url(\"bla.css\");" [lucius|
@import url("bla.css");
|]
    it "lucius simple escapes" $
      celper "*{a:test}" [lucius|
* {
  a: t\65 st;
}
|]
    it "lucius bounded escapes" $
      celper "*{a:teft}" [lucius|
* {
  a: t\000065ft;
}
|]
    it "lucius case-insensitive keywords" $
       celper "@media foo {}" [lucius|
@MeDIa foo {
}
|]
    it "lucius @page statements" $
       celper "@page :right{a:b;c:d}" [lucius|
@page :right {
a:b;
c:d;
}
|]
    it "lucius @font-face statements" $
       celper "@font-face{a:b;c:d}" [lucius|
@font-face {
a:b;
c:d;
}
|]
    it "lucius runtime" $ Right (T.pack "foo {\n    bar: baz;\n}\n") @=? luciusRT (T.pack "foo { bar: #{myvar}}") [(TS.pack "myvar", TS.pack "baz")]
    it "lucius runtime variables" $ Right (T.pack "foo {\n    bar: baz;\n}\n") @=? luciusRT (T.pack "@dummy: dummy; @myvar: baz; @dummy2: dummy; foo { bar: #{myvar}}") []
    it "lucius whtiespace" $ Right (T.pack "@media foo {\n    bar {\n        baz: bin;\n        baz2: bin2;\n    }\n}\n")
      @=? luciusRT (T.pack "@media foo{bar{baz:bin;baz2:bin2}}") []
    it "variables inside value" $
        celper "foo{foo:XbarY}" [lucius|
@bar: bar;
foo { foo:X#{bar}Y; }
|]
    it "variables in media selector" $
        celper "@media (max-width: 400px){foo{color:red}}" [lucius|
@mobileWidth: 400px;
@media (max-width: #{mobileWidth}){ foo { color: red; } }
|]
    -- note: this file format is window (CR;NL)
    it "variables in supports selector" $
        celper "@supports ((perspective: 1px)\r\n           and (not (-webkit-overflow-scrolling: touch))) {html,body{overflow:hidden;height:100%}body{transform:translateZ(0px)}}" [lucius|
@perspectiveTestValue: 1px;
@supports ((perspective: #{perspectiveTestValue})
           and (not (-webkit-overflow-scrolling: touch))) {
    html, body {
        overflow: hidden;
        height:100%;
    }
    body {
        transform: translateZ(0px);
    }
}
|]
    it "URLs in import" $ celper
        "@import url(\"suburl\");" [lucius|
@import url("@{Sub SubUrl}");
|]
    it "vars in charset" $ do
      let charset = "mycharset"
      celper "@charset mycharset;" [lucius|
@charset #{charset};
|]
    it "keyframes" $ celper
        "@keyframes mymove {from{top:0px}to{top:200px}}" [lucius|
@keyframes mymove {
    from {
        top: 0px;
    }
    to {
        top: 200px;
    }
}
|]
    it "prefixed keyframes" $ celper
        "@-webkit-keyframes mymove {from{top:0px}to{top:200px}}" [lucius|
@-webkit-keyframes mymove {
    from {
        top: 0px;
    }
    to {
        top: 200px;
    }
}
|]
    it "lucius mixins" $ do
        let bins = [luciusMixin|
                   bin:bin2;
                   foo2 {
                       x: y;
                   }
                   |] :: Mixin
        celper "foo{bar:baz;bin:bin2}foo foo2{x:y}" [lucius|
            foo {
                bar: baz;
                ^{bins}
            }
        |]
    it "cassius mixins" $ do
        let bins = [cassiusMixin|
                   bin:bin2
                   bin3:bin4

                   foo2
                       x:y
                   |] :: Mixin
        celper "foo{bar:baz;bin:bin2;bin3:bin4}foo foo2{x:y}" [lucius|
            foo {
                bar: baz;
                ^{bins}
            }
        |]
    it "cassius nested mixins" $ do
        let bins = [cassiusMixin|
                   bin:bin2
                   bin3:bin4
                   |] :: Mixin
            bams = [cassiusMixin|
                   bam:bam2
                   ^{bins}
                   bam3:bam4
                   |] :: Mixin
        celper "foo{bar:baz;bam:bam2;bam3:bam4;bin:bin2;bin3:bin4}" [lucius|
            foo {
                ^{bams}
                bar: baz;
            }
        |]

    it "more complicated mixins" $ do
        let transition val =
                [luciusMixin|
                    -webkit-transition: #{val};
                    -moz-transition: #{val};
                    -ms-transition: #{val};
                    -o-transition: #{val};
                    transition: #{val};
                |]

        celper ".some-class{-webkit-transition:all 4s ease;-moz-transition:all 4s ease;-ms-transition:all 4s ease;-o-transition:all 4s ease;transition:all 4s ease}"
                [lucius|
                    .some-class {
                        ^{transition "all 4s ease"}
                    }
                |]

    it "nested mixin blocks" $ do
        let bar = [luciusMixin|
                  bar {
                     bin:baz;
                  }
                  |]
            foo = [luciusMixin|
                  foo {
                    ^{bar}
                  }
                  |] :: Mixin
        celper "selector foo bar{bin:baz}" [lucius|
            selector {
                ^{foo}
            }
        |]

    it "mixins with pseudoselectors" $ do
        let bar = [luciusMixin|
                  &:hover {
                     bin:baz;
                  }
                  |] :: Mixin
        celper "foo:hover{bin:baz}" [lucius|
            foo {
                ^{bar}
            }
        |]

    it "mixins insane structure" $ do
        let bar = [luciusMixin|
                    bar1:bar2;
                    ^{bim}
                    &:hover {
                        ^{bim}
                        jam1:jam2;
                        ^{bam}
                    }
                    
                    ram {
                        ram1:ram2;
                        ^{bim}
                        ^{bam}
                    }
                    ^{bam}
                    bar3:bar4;
                  |] :: Mixin
            bim = [luciusMixin|
                    bim1:bim2;
                    bim-block1 {
                      bim3:bim4;
                      bim5:bim6;
                    }
                    bim-block2 {
                      bim7:bim8;
                      bim9:bim10;
                    }
                  |]
            bam = [luciusMixin|
                    bam1:bam2;
                    bam-block1 {
                      bam3:bam4;
                      bam5:bam6;
                    }
                    bam-block2 {
                      bam7:bam8;
                      bam9:bam10;
                    }
                  |]
                  
        celper
            -- 1. attributes
            -- 2. mixins attributes 
            -- 3. blocks
            -- 4. mixins blocks
            "foo{foo1:foo2;foo3:foo4;bar1:bar2;bar3:bar4;bim1:bim2;bam1:bam2}\

            \foo foo-block1{bar1:bar2;bar3:bar4;bim1:bim2;bam1:bam2}\
            \foo foo-block1:hover{jam1:jam2;bim1:bim2;bam1:bam2}\
            \foo foo-block1:hover bim-block1{bim3:bim4;bim5:bim6}\
            \foo foo-block1:hover bim-block2{bim7:bim8;bim9:bim10}\
            \foo foo-block1:hover bam-block1{bam3:bam4;bam5:bam6}\
            \foo foo-block1:hover bam-block2{bam7:bam8;bam9:bam10}\
            \foo foo-block1 ram{ram1:ram2;bim1:bim2;bam1:bam2}\
            \foo foo-block1 ram bim-block1{bim3:bim4;bim5:bim6}\
            \foo foo-block1 ram bim-block2{bim7:bim8;bim9:bim10}\
            \foo foo-block1 ram bam-block1{bam3:bam4;bam5:bam6}\
            \foo foo-block1 ram bam-block2{bam7:bam8;bam9:bam10}\
            \foo foo-block1 bim-block1{bim3:bim4;bim5:bim6}\
            \foo foo-block1 bim-block2{bim7:bim8;bim9:bim10}\
            \foo foo-block1 bam-block1{bam3:bam4;bam5:bam6}\
            \foo foo-block1 bam-block2{bam7:bam8;bam9:bam10}\

            \foo foo-block2{bar1:bar2;bar3:bar4;bim1:bim2;bam1:bam2}\
            \foo foo-block2:hover{jam1:jam2;bim1:bim2;bam1:bam2}\
            \foo foo-block2:hover bim-block1{bim3:bim4;bim5:bim6}\
            \foo foo-block2:hover bim-block2{bim7:bim8;bim9:bim10}\
            \foo foo-block2:hover bam-block1{bam3:bam4;bam5:bam6}\
            \foo foo-block2:hover bam-block2{bam7:bam8;bam9:bam10}\
            \foo foo-block2 ram{ram1:ram2;bim1:bim2;bam1:bam2}\
            \foo foo-block2 ram bim-block1{bim3:bim4;bim5:bim6}\
            \foo foo-block2 ram bim-block2{bim7:bim8;bim9:bim10}\
            \foo foo-block2 ram bam-block1{bam3:bam4;bam5:bam6}\
            \foo foo-block2 ram bam-block2{bam7:bam8;bam9:bam10}\
            \foo foo-block2 bim-block1{bim3:bim4;bim5:bim6}\
            \foo foo-block2 bim-block2{bim7:bim8;bim9:bim10}\
            \foo foo-block2 bam-block1{bam3:bam4;bam5:bam6}\
            \foo foo-block2 bam-block2{bam7:bam8;bam9:bam10}\

            \foo:hover{jam1:jam2;bim1:bim2;bam1:bam2}\
            \foo:hover bim-block1{bim3:bim4;bim5:bim6}\
            \foo:hover bim-block2{bim7:bim8;bim9:bim10}\
            \foo:hover bam-block1{bam3:bam4;bam5:bam6}\
            \foo:hover bam-block2{bam7:bam8;bam9:bam10}\
            \foo ram{ram1:ram2;bim1:bim2;bam1:bam2}\
            \foo ram bim-block1{bim3:bim4;bim5:bim6}\
            \foo ram bim-block2{bim7:bim8;bim9:bim10}\
            \foo ram bam-block1{bam3:bam4;bam5:bam6}\
            \foo ram bam-block2{bam7:bam8;bam9:bam10}\
            \foo bim-block1{bim3:bim4;bim5:bim6}\
            \foo bim-block2{bim7:bim8;bim9:bim10}\
            \foo bam-block1{bam3:bam4;bam5:bam6}\
            \foo bam-block2{bam7:bam8;bam9:bam10}"
            [lucius|
                foo {
                    foo1:foo2;
                    ^{bar}
                    foo-block1 {
                        ^{bar}
                    }
                    foo-block2 {
                        ^{bar}
                    }
                    foo3:foo4;
                }
                |]

    it "runtime mixin" $ do
        let bins = [luciusMixin|
                   bin:bin2;
                   foo2 {
                       x: y;
                   }
                   |] :: Mixin
        Right (T.pack "foo{bar:baz;bin:bin2}foo foo2{x:y}") @=? luciusRTMixin
            (T.pack "foo { bar: baz; ^{bins} }")
            True
            [(TS.pack "bins", RTVMixin bins)]

    it "luciusFileReload mixin" $ do
      let mixin = [luciusMixin|foo:bar;baz:bin|]
      flip celper $(luciusFileReload "test/cassiuses/mixin.lucius") $ concat
          [ "selector {\n    foo: bar;\n    baz: bin;\n}\n"
          ]

    it "cassiusFileReload with import URL" $ do
      celper
        "@import url(url);\n"
        $(cassiusFileReload "test/cassiuses/reload-import.cassius")

    it "& subblocks" $
        celper "foo:bar{baz:bin}"
        [lucius|
            foo {
                &:bar {
                    baz: bin;
                }
            }
        |]

    describe "font-face #139" $ do
        it "lucius" $
            celper "@font-face{font-family:myFirstFont;src:url(sansation_light.woff)}"
            [lucius|
                @font-face {
                    font-family: myFirstFont;
                    src: url(sansation_light.woff);
                }
            |]
        it "cassius" $
            celper "@font-face{font-family:myFirstFont;src:url(sansation_light.woff)}"
            [cassius|
                @font-face
                    font-family: myFirstFont
                    src: url(sansation_light.woff)
            |]

    describe "trailing semicolon in mixin" $ do
        let someMixin = [luciusMixin|foo:bar|]
        it "direct in lucius" $
            celper "baz{foo:bar}" [lucius|
                baz {
                    ^{someMixin};
                }
            |]

        it "implicit in cassius #194" $
            celper "baz{foo:bar}" [cassius|
                baz
                    ^{someMixin}
            |]

  describe "Ordered parsers" $ do
    it "Ordered.cassius" caseCassiusOrd
    it "Ordered.cassiusFile" caseCassiusFileOrd
    it "Ordered.cassius single comment" caseCassiusSingleCommentOrd
    it "Ordered.cassius leading comment" caseCassiusLeadingCommentOrd

    it "cassiusFileDebug" $ do
      let var = "var"
      let selector = "foo"
      let urlp = (Home, [(pack "p", pack "q")])
      flip celper $(cassiusFileDebug "test/cassiuses/external1.cassius") $ concat
          [ "foo {\n    background: #000;\n    bar: baz;\n    color: #F00;\n}\n"
          , "bin {\n"
          , "    background-image: url(url);\n"
          , "    bar: bar;\n    color: #7F6405;\n    fvarx: someval;\n    unicode-test: שלום;\n"
          , "    urlp: url(url?p=q);\n}\n"
          ]

{- TODO
    it "cassiusFileDebugChange" $ do
    let var = "var"
    writeFile "test/cassiuses/external2.cassius" "foo\n  #{var}: 1"
    celper "foo{var:1}" $(cassiusFileDebug "test/cassiuses/external2.cassius")
    writeFile "test/cassiuses/external2.cassius" "foo\n  #{var}: 2"
    celper "foo{var:2}" $(cassiusFileDebug "test/cassiuses/external2.cassius")
    writeFile "test/cassiuses/external2.cassius" "foo\n  #{var}: 1"
    -}

  describe "Ordered.cassius" $ do
    it "comments" $ do
      -- FIXME reconsider Hamlet comment syntax?
      celper "" [Ordered.cassius|/* this is a comment */
/* another comment */
/*a third one*/|]

    it "pseudo-class" $
      flip celper [Ordered.cassius|
a:visited
    color: blue
|] "a:visited{color:blue}"


    it "ignores a blank line" $ do
      celper "foo{bar:baz}" [Ordered.cassius|
foo

    bar: baz

|]


    it "leading spaces" $
      celper "foo{bar:baz}" [Ordered.cassius|
  foo
    bar: baz
|]


    it "all spaces" $
      celper "h1{color:green }" [Ordered.cassius|
    h1
        color: green 
    |]


    it "whitespace and colons" $ do
      celper "h1:hover{color:green ;font-family:sans-serif}" [Ordered.cassius|
    h1:hover
        color: green 
        font-family:sans-serif
    |]


    it "trailing comments" $
      celper "h1:hover{color:green ;font-family:sans-serif}" [Ordered.cassius|
    h1:hover /* Please ignore this */
        color: green /* This is a comment. */
        /* Obviously this is ignored too. */
        font-family:sans-serif
    |]

    it "nesting" $
      celper "foo bar{baz:bin}" [Ordered.cassius|
    foo
        bar
            baz: bin
    |]

    it "variable" $
      celper "foo bar{baz:bin}" [Ordered.cassius|
    @binvar: bin
    foo
        bar
            baz: #{binvar}
    |]

    it "trailing semicolon" $
      celper "foo bar{baz:bin}" [Ordered.cassius|
    @binvar: bin
    foo
        bar
            baz: #{binvar};
    |]


    it "module names" $ do
      let foo = "foo"
          dub = 3.14::Double
          int = -5::Int
      celper "sel{bar:oof oof 3.14 -5}"
        [Ordered.cassius|
sel
    bar: #{Data.List.reverse foo} #{L.reverse foo} #{show dub} #{show int}
|]


    it "single dollar at and caret" $ do
      celper "sel{att:$@^}" [Ordered.cassius|
sel
    att: $@^
|]

  {-
      celper "sel{att:#{@{^{}" [cassius|
sel
    att: #\{@\{^{
|]
-}


    it "dollar operator" $ do
      let val = (1, (2, 3)) :: (Integer, (Integer, Integer))
      celper "sel{att:2}" [Ordered.cassius|
sel
    att: #{ show $ fst $ snd val }
|]
      celper "sel{att:2}" [Ordered.cassius|
sel
    att: #{ show $ fst $ snd $ val}
|]


    it "embedded slash" $ do
      celper "sel{att:///}" [Ordered.cassius|
sel
    att: ///
|]


    it "multi cassius" $ do
      celper "foo{bar:baz;bar:bin}" [Ordered.cassius|
foo
    bar: baz
    bar: bin
|]

  describe "Ordered.lucius" $ do
    it "lucius" $ do
      let var = "var"
      let urlp = (Home, [(pack "p", pack "q")])
      flip celper [Ordered.lucius|
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

    it "lucius file" $ do
      let var = "var"
      let urlp = (Home, [(pack "p", pack "q")])
      flip celper $(Ordered.luciusFile "test/cassiuses/external1.lucius") $ concat
          [ "foo{background:#000;bar:baz;color:#F00}"
          , "bin{"
          , "background-image:url(url);"
          , "bar:bar;color:#7F6405;fvarx:someval;unicode-test:שלום;"
          , "urlp:url(url?p=q)}"
          ]

    it "lucius file debug" caseLuciusFileDebugOrd

    it "lucius nested" $ do
      celper "foo bar{baz:bin}" $(Ordered.luciusFile "test/cassiuses/external-nested.lucius")
      celper "foo bar {\n    baz: bin;\n}\n" $(luciusFileDebug "test/cassiuses/external-nested.lucius")
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


    it "lucius charset" $ do
      celper (concat ["@charset \"utf-8\";"
        , "#content ul{list-style:none;padding:0 5em}"
        , "#content ul li{padding:1em 0}"
        , "#content ul li a{color:#419a56;font-family:'TeXGyreHerosBold',helvetica,arial,sans-serif;font-weight:bold;text-transform:uppercase;white-space:nowrap}"
        ]) [Ordered.lucius|
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

    it "lucius media" $ do
      celper "@media only screen{foo bar{baz:bin}}" $(Ordered.luciusFile "test/cassiuses/external-media.lucius")
      celper "@media only screen {\n    foo bar {\n        baz: bin;\n    }\n}\n" $(Ordered.luciusFileDebug "test/cassiuses/external-media.lucius")
      celper "@media only screen{foo bar{baz:bin}}" [Ordered.lucius|
        @media only screen{
            foo {
                bar {
                    baz: bin;
                }
            }
        }
        |]

    it "lucius supports" $ do
      celper "@supports only screen{hana dul{set:net}}" $(Ordered.luciusFile "test/cassiuses/external-supports.lucius")
      celper "@supports only screen {\n    hana dul {\n        set: net;\n    }\n}\n" $(Ordered.luciusFileDebug "test/cassiuses/external-supports.lucius")
      celper "@supports only screen    {hana,dul{set:net;dasut:yeosut}}" [Ordered.lucius|
        @supports only screen    {
            hana, dul {
                set: net;
                dasut: yeosut;
            }
        }
        |]

    it "lucius @layer block (ordered)" $
      celper "@layer utilities{.padding{padding:1rem}}" [Ordered.lucius|
        @layer utilities{
            .padding { padding: 1rem; }
        }
        |]

    it "lucius @layer declaration (ordered)" $
      celper "@layer utilities;" [Ordered.lucius|
        @layer utilities;
        |]

    it "lucius @layer multiple (ordered)" $
      celper "@layer utilities, components;" [Ordered.lucius|
        @layer utilities, components;
        |]

    it "lucius @layer anonymous (ordered)" $
      celper "@layer {.padding{padding:1rem}}" [Ordered.lucius|
        @layer {
            .padding { padding: 1rem; }
        }
        |]

    it "lucius @media inside @layer (ordered)" $
      celper "@layer components {@media only screen {.btn{font-size:1.5rem}}}" [Ordered.lucius|
        @layer components {
            @media only screen {
                .btn { font-size: 1.5rem; }
            }
        }
        |]

    it "lucius @layer inside @media (ordered)" $
      celper "@media (max-width: 600px) {@layer utilities {.hidden{display:none}}}" [Ordered.lucius|
        @media (max-width: 600px) {
            @layer utilities {
                .hidden { display: none; }
            }
        }
        |]

    it "lucius @supports inside @layer (ordered)" $
      celper "@layer base {@supports (display: grid) {.grid{display:grid}}}" [Ordered.lucius|
        @layer base {
            @supports (display: grid) {
                .grid { display: grid; }
            }
        }
        |]

    it "lucius @media inside @media (ordered)" $
      celper "@media screen {@media (min-width: 768px) {.container{max-width:720px}}}" [Ordered.lucius|
        @media screen {
            @media (min-width: 768px) {
                .container { max-width: 720px; }
            }
        }
        |]

    {-
    it "cassius removes whitespace" $ do
      celper "foo{bar:baz}" [cassius|
      foo
          bar     :    baz
      |]
      -}

    it "lucius trailing comments" $
      celper "foo{bar:baz}" [Ordered.lucius|foo{bar:baz;}/* ignored*/|]

    it "lucius variables" $ celper "foo{bar:baz}" [lucius|
@myvar: baz;
foo {
    bar: #{myvar};
}
|]
    it "lucius CDO/CDC tokens" $
       celper "*{a:b}" [lucius|
<!-- --> <!--
* {
  a: b;
}
-->
|]
    it "lucius @import statements" $
      celper "@import url(\"bla.css\");" [Ordered.lucius|
@import url("bla.css");
|]
    it "lucius simple escapes" $
      celper "*{a:test}" [Ordered.lucius|
* {
  a: t\65 st;
}
|]
    it "lucius bounded escapes" $
      celper "*{a:teft}" [Ordered.lucius|
* {
  a: t\000065ft;
}
|]
    it "lucius case-insensitive keywords" $
       celper "@media foo {}" [Ordered.lucius|
@MeDIa foo {
}
|]
    it "lucius @page statements" $
       celper "@page :right{a:b;c:d}" [Ordered.lucius|
@page :right {
a:b;
c:d;
}
|]
    it "lucius @font-face statements" $
       celper "@font-face{a:b;c:d}" [Ordered.lucius|
@font-face {
a:b;
c:d;
}
|]
    it "lucius runtime" $ Right (T.pack "foo {\n    bar: baz;\n}\n") @=? Ordered.luciusRT (T.pack "foo { bar: #{myvar}}") [(TS.pack "myvar", TS.pack "baz")]
    it "lucius runtime variables" $ Right (T.pack "foo {\n    bar: baz;\n}\n") @=? Ordered.luciusRT (T.pack "@dummy: dummy; @myvar: baz; @dummy2: dummy; foo { bar: #{myvar}}") []
    it "lucius whtiespace" $ Right (T.pack "@media foo {\n    bar {\n        baz: bin;\n        baz2: bin2;\n    }\n}\n")
      @=? Ordered.luciusRT (T.pack "@media foo{bar{baz:bin;baz2:bin2}}") []
    it "variables inside value" $
        celper "foo{foo:XbarY}" [Ordered.lucius|
@bar: bar;
foo { foo:X#{bar}Y; }
|]
    it "variables in media selector" $
        celper "@media (max-width: 400px){foo{color:red}}" [Ordered.lucius|
@mobileWidth: 400px;
@media (max-width: #{mobileWidth}){ foo { color: red; } }
|]
    -- note: this file format is window (CR;NL)
    it "variables in supports selector" $
        celper "@supports ((perspective: 1px)\r\n           and (not (-webkit-overflow-scrolling: touch))) {html,body{overflow:hidden;height:100%}body{transform:translateZ(0px)}}" [Ordered.lucius|
@perspectiveTestValue: 1px;
@supports ((perspective: #{perspectiveTestValue})
           and (not (-webkit-overflow-scrolling: touch))) {
    html, body {
        overflow: hidden;
        height:100%;
    }
    body {
        transform: translateZ(0px);
    }
}
|]
    it "URLs in import" $ celper
        "@import url(\"suburl\");" [Ordered.lucius|
@import url("@{Sub SubUrl}");
|]
    it "vars in charset" $ do
      let charset = "mycharset"
      celper "@charset mycharset;" [Ordered.lucius|
@charset #{charset};
|]
    it "keyframes" $ celper
        "@keyframes mymove {from{top:0px}to{top:200px}}" [Ordered.lucius|
@keyframes mymove {
    from {
        top: 0px;
    }
    to {
        top: 200px;
    }
}
|]
    it "prefixed keyframes" $ celper
        "@-webkit-keyframes mymove {from{top:0px}to{top:200px}}" [Ordered.lucius|
@-webkit-keyframes mymove {
    from {
        top: 0px;
    }
    to {
        top: 200px;
    }
}
|]

  describe "ordered mixins" $ do
    it "lucius mixins" $ do
        let bins = [Ordered.luciusMixin|
                   bin:bin2;
                   foo2 {
                        x: y;
                    }
                   |] :: Mixin

        celper "foo{bar:baz;bin:bin2}foo foo2{x:y}" [Ordered.lucius|
            foo {
                bar: baz;
                ^{bins}
            }
        |]
    it "lucius mixins keep order" $ do
        let bins = [Ordered.luciusMixin|
                   bin:bin2;
                   foo2 {
                       x: y;
                   }
                   |] :: Mixin
        celper "foo{bar1:baz1;bin:bin2;bar2:baz2}foo foo2{x:y}" [Ordered.lucius|
            foo {
                bar1:baz1;
                ^{bins}
                bar2: baz2;
            }
        |]

    it "lucius nested mixins" $ do
        let bins = [Ordered.luciusMixin|
                   bin:bin2;
                   bin3:bin4;
                   |] :: Mixin
            bams = [Ordered.luciusMixin|
                   bam:bam2;
                   ^{bins}
                   bam3:bam4;
                   |] :: Mixin
        celper "foo{bar:bar2;bam:bam2;bin:bin2;bin3:bin4;bam3:bam4;bar3:bar4}" [Ordered.lucius|
            foo {
                bar:bar2;
                ^{bams}
                bar3: bar4;
            }
        |]

    it "cassius mixins" $ do
        let bins = [Ordered.cassiusMixin|
                   bin:bin2
                   bin3:bin4
                   |] :: Mixin
        -- No sublocks celper "foo{bar:baz;bin:bin2}foo foo2{x:y}" [lucius|
        celper "foo{bar:baz;bin:bin2;bin3:bin4}" [Ordered.lucius|
            foo {
                bar: baz;
                ^{bins}
            }
        |]

    it "cassius mixins keep order" $ do
        let bins = [Ordered.cassiusMixin|
                   bin:bin2
                   bin3:bin4
                   |] :: Mixin
        -- No sublocks celper "foo{bar:baz;bin:bin2}foo foo2{x:y}" [lucius|
        celper "foo{bar1:baz1;bin:bin2;bin3:bin4;bar2:baz2}" [Ordered.lucius|
            foo {
                bar1: baz1;
                ^{bins}
                bar2:baz2;
            }
        |]

    it "cassius nested mixins" $ do
        let bins = [Ordered.cassiusMixin|
                   bin:bin2
                   bin3:bin4
                   |] :: Mixin
            bams = [Ordered.cassiusMixin|
                   bam:bam2
                   ^{bins}
                   bam3:bam4
                   |] :: Mixin
        celper "foo{bar:bar2;bam:bam2;bin:bin2;bin3:bin4;bam3:bam4;bar3:bar4}" [Ordered.lucius|
            foo {
                bar:bar2;
                ^{bams}
                bar3: bar4;
            }
        |]

    it "more complicated mixins" $ do
        let transition val =
                [Ordered.luciusMixin|
                    -webkit-transition: #{val};
                    -moz-transition: #{val};
                    -ms-transition: #{val};
                    -o-transition: #{val};
                    transition: #{val};
                |]

        celper ".some-class{-webkit-transition:all 4s ease;-moz-transition:all 4s ease;-ms-transition:all 4s ease;-o-transition:all 4s ease;transition:all 4s ease}"
                [Ordered.lucius|
                    .some-class {
                        ^{transition "all 4s ease"}
                    }
                |]

    it "nested mixin blocks" $ do
        let bar = [Ordered.luciusMixin|
                    bar {
                        bin:baz;
                    }
                  |]
            foo = [luciusMixin|
                    foo {
                        ^{bar}
                    }
                  |] :: Mixin
        celper "selector foo bar{bin:baz}" [Ordered.lucius|
            selector {
                ^{foo}
            }
        |]

    it "mixins with pseudoselectors" $ do
        let bar = [Ordered.luciusMixin|
                    &:hover {
                        bin:baz;
                    }
                  |] :: Mixin
        celper "foo:hover{bin:baz}" [Ordered.lucius|
            foo {
                ^{bar}
            }
        |]

    it "mixins insane structure" $ do
        let bar = [Ordered.luciusMixin|
                    bar1:bar2;
                    ^{bim}
                    &:hover {
                        ^{bim}
                        jam1:jam2;
                        ^{bam}
                    }
                    
                    ram {
                        ^{bim}
                        ram1:ram2;
                        ^{bam}
                    }
                    ^{bam}
                    bar3:bar4;
                  |] :: Mixin
            bim = [Ordered.luciusMixin|
                    bim1:bim2;
                    bim-block1 {
                      bim3:bim4;
                      bim5:bim6;
                    }
                    bim-block2 {
                      bim7:bim8;
                      bim9:bim10;
                    }
                  |]
            bam = [Ordered.luciusMixin|
                    bam1:bam2;
                    bam-block1 {
                      bam3:bam4;
                      bam5:bam6;
                    }
                    bam-block2 {
                      bam7:bam8;
                      bam9:bam10;
                    }
                  |]
                  
        celper
            -- 1. ordered attributes and mixins attributes 
            -- 2. blocks
            -- 3. mixins blocks
            "foo{foo1:foo2;bar1:bar2;bim1:bim2;bam1:bam2;bar3:bar4;foo3:foo4}\

            \foo foo-block1{bar1:bar2;bim1:bim2;bam1:bam2;bar3:bar4}\
            \foo foo-block1:hover{bim1:bim2;jam1:jam2;bam1:bam2}\
            \foo foo-block1:hover bim-block1{bim3:bim4;bim5:bim6}\
            \foo foo-block1:hover bim-block2{bim7:bim8;bim9:bim10}\
            \foo foo-block1:hover bam-block1{bam3:bam4;bam5:bam6}\
            \foo foo-block1:hover bam-block2{bam7:bam8;bam9:bam10}\
            \foo foo-block1 ram{bim1:bim2;ram1:ram2;bam1:bam2}\
            \foo foo-block1 ram bim-block1{bim3:bim4;bim5:bim6}\
            \foo foo-block1 ram bim-block2{bim7:bim8;bim9:bim10}\
            \foo foo-block1 ram bam-block1{bam3:bam4;bam5:bam6}\
            \foo foo-block1 ram bam-block2{bam7:bam8;bam9:bam10}\
            \foo foo-block1 bim-block1{bim3:bim4;bim5:bim6}\
            \foo foo-block1 bim-block2{bim7:bim8;bim9:bim10}\
            \foo foo-block1 bam-block1{bam3:bam4;bam5:bam6}\
            \foo foo-block1 bam-block2{bam7:bam8;bam9:bam10}\

            \foo foo-block2{bar1:bar2;bim1:bim2;bam1:bam2;bar3:bar4}\
            \foo foo-block2:hover{bim1:bim2;jam1:jam2;bam1:bam2}\
            \foo foo-block2:hover bim-block1{bim3:bim4;bim5:bim6}\
            \foo foo-block2:hover bim-block2{bim7:bim8;bim9:bim10}\
            \foo foo-block2:hover bam-block1{bam3:bam4;bam5:bam6}\
            \foo foo-block2:hover bam-block2{bam7:bam8;bam9:bam10}\
            \foo foo-block2 ram{bim1:bim2;ram1:ram2;bam1:bam2}\
            \foo foo-block2 ram bim-block1{bim3:bim4;bim5:bim6}\
            \foo foo-block2 ram bim-block2{bim7:bim8;bim9:bim10}\
            \foo foo-block2 ram bam-block1{bam3:bam4;bam5:bam6}\
            \foo foo-block2 ram bam-block2{bam7:bam8;bam9:bam10}\
            \foo foo-block2 bim-block1{bim3:bim4;bim5:bim6}\
            \foo foo-block2 bim-block2{bim7:bim8;bim9:bim10}\
            \foo foo-block2 bam-block1{bam3:bam4;bam5:bam6}\
            \foo foo-block2 bam-block2{bam7:bam8;bam9:bam10}\

            \foo:hover{bim1:bim2;jam1:jam2;bam1:bam2}\
            \foo:hover bim-block1{bim3:bim4;bim5:bim6}\
            \foo:hover bim-block2{bim7:bim8;bim9:bim10}\
            \foo:hover bam-block1{bam3:bam4;bam5:bam6}\
            \foo:hover bam-block2{bam7:bam8;bam9:bam10}\
            \foo ram{bim1:bim2;ram1:ram2;bam1:bam2}\
            \foo ram bim-block1{bim3:bim4;bim5:bim6}\
            \foo ram bim-block2{bim7:bim8;bim9:bim10}\
            \foo ram bam-block1{bam3:bam4;bam5:bam6}\
            \foo ram bam-block2{bam7:bam8;bam9:bam10}\
            \foo bim-block1{bim3:bim4;bim5:bim6}\
            \foo bim-block2{bim7:bim8;bim9:bim10}\
            \foo bam-block1{bam3:bam4;bam5:bam6}\
            \foo bam-block2{bam7:bam8;bam9:bam10}"
            [Ordered.lucius|
                foo {
                    foo1:foo2;
                    ^{bar}
                    foo-block1 {
                        ^{bar}
                    }
                    foo-block2 {
                        ^{bar}
                    }
                    foo3:foo4;
                }
            |]

    it "runtime mixin" $ do
        let bins = [Ordered.luciusMixin|
                   bin:bin2;
                   foo2 {
                       x: y;
                   }
                   |] :: Mixin
        Right (T.pack "foo{bar:baz;bin:bin2}foo foo2{x:y}") @=? Ordered.luciusRTMixin
            (T.pack "foo { bar: baz; ^{bins} }")
            True
            [(TS.pack "bins", RTVMixin bins)]

    it "luciusFileReload mixin" $ do
      let mixin = [Ordered.luciusMixin|foo:bar;baz:bin|]
      flip celper $(Ordered.luciusFileReload "test/cassiuses/mixin.lucius") $ concat
          [ "selector {\n    foo: bar;\n    baz: bin;\n}\n"
          ]

    it "cassiusFileReload with import URL" $ do
      celper
        "@import url(url);\n"
        $(Ordered.cassiusFileReload "test/cassiuses/reload-import.cassius")

    it "& subblocks" $
        celper "foo:bar{baz:bin}"
        [Ordered.lucius|
            foo {
                &:bar {
                    baz: bin;
                }
            }
        |]

    describe "font-face #139" $ do
        it "lucius" $
            celper "@font-face{font-family:myFirstFont;src:url(sansation_light.woff)}"
            [Ordered.lucius|
                @font-face {
                    font-family: myFirstFont;
                    src: url(sansation_light.woff);
                }
            |]
        it "cassius" $
            celper "@font-face{font-family:myFirstFont;src:url(sansation_light.woff)}"
            [Ordered.cassius|
                @font-face
                    font-family: myFirstFont
                    src: url(sansation_light.woff)
            |]

    describe "trailing semicolon in mixin" $ do
        let someMixin = [Ordered.luciusMixin|foo:bar|]
        it "direct in lucius" $
            celper "baz{foo:bar}" [Ordered.lucius|
                baz {
                    ^{someMixin};
                }
            |]

        it "implicit in cassius #194" $
            celper "baz{foo:bar}" [Ordered.cassius|
                baz
                    ^{someMixin}
            |]

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



celper :: HasCallStack => String -> CssUrl Url -> Assertion
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

caseCassiusOrd :: Assertion
caseCassiusOrd = do
    let var = "var"
    let urlp = (Home, [(pack "p", pack "q")])
    flip celper [Ordered.cassius|
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

caseCassiusFileOrd :: Assertion
caseCassiusFileOrd = do
    let var = "var"
    let selector = "foo"
    let urlp = (Home, [(pack "p", pack "q")])
    flip celper $(Ordered.cassiusFile "test/cassiuses/external1.cassius") $ concat
        [ "foo{background:#000;bar:baz;color:#F00}"
        , "bin{"
        , "background-image:url(url);"
        , "bar:bar;color:#7F6405;fvarx:someval;unicode-test:שלום;"
        , "urlp:url(url?p=q)}"
        ]

caseCassiusSingleComment :: Assertion
caseCassiusSingleComment =
    flip celper [cassius|
        /*
        this is a comment
        */
        |] ""

caseCassiusSingleCommentOrd :: Assertion
caseCassiusSingleCommentOrd =
    flip celper [Ordered.cassius|
        /*
        this is a comment
        */
        |] ""

caseCassiusLeadingComment :: Assertion
caseCassiusLeadingComment =
    flip celper [cassius|
        /*
        this is a comment
        */
        sel1
            foo: bar
        sel2
            baz: bin
        |] "sel1{foo:bar}sel2{baz:bin}"

caseCassiusLeadingCommentOrd :: Assertion
caseCassiusLeadingCommentOrd =
    flip celper [Ordered.cassius|
        /*
        this is a comment
        */
        sel1
            foo: bar
        sel2
            baz: bin
        |] "sel1{foo:bar}sel2{baz:bin}"

instance Show Url where
    show _ = "FIXME remove this instance show Url"


caseLuciusFileDebug :: Assertion
caseLuciusFileDebug = do
    let var = "var"
    writeFile "test/cassiuses/external2.lucius" "foo{#{var}: 1}"
    celper "foo {\n    var: 1;\n}\n" $(luciusFileDebug "test/cassiuses/external2.lucius")
    writeFile "test/cassiuses/external2.lucius" "foo{#{var}: 2}"
    celper "foo {\n    var: 2;\n}\n" $(luciusFileDebug "test/cassiuses/external2.lucius")
    writeFile "test/cassiuses/external2.lucius" "foo{#{var}: 1}"

caseLuciusFileDebugOrd :: Assertion
caseLuciusFileDebugOrd = do
    let var = "var"
    writeFile "test/cassiuses/external2.lucius" "foo{#{var}: 1}"
    celper "foo {\n    var: 1;\n}\n" $(Ordered.luciusFileDebug "test/cassiuses/external2.lucius")
    writeFile "test/cassiuses/external2.lucius" "foo{#{var}: 2}"
    celper "foo {\n    var: 2;\n}\n" $(Ordered.luciusFileDebug "test/cassiuses/external2.lucius")
    writeFile "test/cassiuses/external2.lucius" "foo{#{var}: 1}"
