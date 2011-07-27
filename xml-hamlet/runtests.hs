{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
import Text.Hamlet.XML
import qualified Text.XML.Enumerator.Resolved as X
import Test.HUnit
import Test.Hspec
import Test.Hspec.HUnit ()

main :: IO ()
main = hspecX $ describe "xml-hamlet"
    [ it "handles plain tags" $ [xml|
<foo>
        <baz
|] @?=
        [ X.NodeElement $ X.Element "foo" []
            [ X.NodeElement $ X.Element "baz" [] []
            ]
        ]
    , it "handles raw text" $ [xml|
<foo>
        <baz>bin
|] @?=
        [ X.NodeElement $ X.Element "foo" []
            [ X.NodeElement $ X.Element "baz" []
                [ X.NodeContent "bin"
                ]
            ]
        ]
    , it "handles variables" $ [xml|
<foo>
        <baz>#{bin}
|] @?=
        [ X.NodeElement $ X.Element "foo" []
            [ X.NodeElement $ X.Element "baz" []
                [ X.NodeContent "bin"
                ]
            ]
        ]
    , it "handles embed" $ [xml|
<foo>
        <baz>^{nodes}
|] @?=
        [ X.NodeElement $ X.Element "foo" []
            [ X.NodeElement $ X.Element "baz" [] nodes
            ]
        ]
    , it "handles attributes" $ [xml|
<foo>
    <bar here=there>
        <baz
|] @?=
        [ X.NodeElement $ X.Element "foo" []
            [ X.NodeElement $ X.Element "bar" [("here", "there")]
                [ X.NodeElement $ X.Element "baz" [] []
                ]
            ]
        ]
    , it "handles attributes" $ [xml|
<foo>
    <bar here=there>
        <baz :False:false=false :True:true=#{true}
|] @?=
        [ X.NodeElement $ X.Element "foo" []
            [ X.NodeElement $ X.Element "bar" [("here", "there")]
                [ X.NodeElement $ X.Element "baz" [("true", "true")] []
                ]
            ]
        ]
    , it "handles forall" $ [xml|
$forall x <- xs
    <word>#{x}
    |] @?=
        [ X.NodeElement $ X.Element "word" [] [X.NodeContent "foo"]
        , X.NodeElement $ X.Element "word" [] [X.NodeContent "bar"]
        , X.NodeElement $ X.Element "word" [] [X.NodeContent "baz"]
        ]
    , it "handles with" $ [xml|
$with ys <- xs
    $forall x <- ys
        <word>#{x}
    |] @?=
        [ X.NodeElement $ X.Element "word" [] [X.NodeContent "foo"]
        , X.NodeElement $ X.Element "word" [] [X.NodeContent "bar"]
        , X.NodeElement $ X.Element "word" [] [X.NodeContent "baz"]
        ]
    , it "handles maybe" $ [xml|
$maybe x <- Just 5
    <one>
$nothing
    <two>
$maybe x <- Nothing
    <three>
$nothing
    <four>
    |] @?=
        [ X.NodeElement $ X.Element "one" [] []
        , X.NodeElement $ X.Element "four" [] []
        ]
    , it "handles conditionals" $ [xml|
$if True
    <one>
$else
    <two>

$if False
    <three>
$elseif True
    <four>

$if False
    <five>
$elseif False
    <six>
$else
    <seven>
|] @?=
        [ X.NodeElement $ X.Element "one" [] []
        , X.NodeElement $ X.Element "four" [] []
        , X.NodeElement $ X.Element "seven" [] []
        ]
    , it "recognizes clark notation" $ [xml|
<{foo}bar {baz}bin="x"
|] @?= [X.NodeElement $ X.Element "{foo}bar" [("{baz}bin", "x")] []]
    ]
  where
    bin = "bin"
    nodes = [X.NodeInstruction $ X.Instruction "ifoo" "ibar"]
    true = "true"
    xs = ["foo", "bar", "baz"]
