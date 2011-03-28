{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Prelude hiding (reverse)
import Text.Hamlet
import Text.Cassius
import Text.Lucius
import Text.Julius
import Text.Coffeescript
import Data.List (intercalate)
import qualified Data.Text.Lazy as T
import qualified Data.List
import qualified Data.List as L
import qualified Data.JSON.Types as J
import qualified Data.Map as Map
import Data.Text.Lazy.Builder (fromText, toLazyText)

main :: IO ()
main = defaultMain [testSuite]

testSuite :: Test
testSuite = testGroup "Text.Hamlet"
    [
      testCase "empty" caseEmpty
    , testCase "static" caseStatic
    , testCase "tag" caseTag
    , testCase "var" caseVar
    , testCase "var chain " caseVarChain
    , testCase "url" caseUrl
    , testCase "url chain " caseUrlChain
    , testCase "embed" caseEmbed
    , testCase "embed chain " caseEmbedChain
    , testCase "if" caseIf
    , testCase "if chain " caseIfChain
    , testCase "else" caseElse
    , testCase "else chain " caseElseChain
    , testCase "elseif" caseElseIf
    , testCase "elseif chain " caseElseIfChain
    , testCase "list" caseList
    , testCase "list chain" caseListChain
    , testCase "script not empty" caseScriptNotEmpty
    , testCase "meta empty" caseMetaEmpty
    , testCase "input empty" caseInputEmpty
    , testCase "multiple classes" caseMultiClass
    , testCase "attrib order" caseAttribOrder
    , testCase "nothing" caseNothing
    , testCase "nothing chain " caseNothingChain
    , testCase "just" caseJust
    , testCase "just chain " caseJustChain
    , testCase "constructor" caseConstructor
    , testCase "url + params" caseUrlParams
    , testCase "escape" caseEscape
    , testCase "empty statement list" caseEmptyStatementList
    , testCase "attribute conditionals" caseAttribCond
    , testCase "non-ascii" caseNonAscii
    , testCase "maybe function" caseMaybeFunction
    , testCase "trailing dollar sign" caseTrailingDollarSign
    , testCase "non leading percent sign" caseNonLeadingPercent
    , testCase "quoted attributes" caseQuotedAttribs
    , testCase "spaced derefs" caseSpacedDerefs
    , testCase "attrib vars" caseAttribVars
    , testCase "strings and html" caseStringsAndHtml
    , testCase "nesting" caseNesting
    , testCase "trailing space" caseTrailingSpace
    , testCase "currency symbols" caseCurrency
    , testCase "external" caseExternal
    , testCase "parens" caseParens
    , testCase "hamlet literals" caseHamletLiterals
    , testCase "hamlet' and xhamlet'" caseHamlet'
    , testCase "hamletDebug" caseHamletDebug
    , testCase "hamlet runtime" caseHamletRT
    , testCase "hamletFileDebug- changing file" caseHamletFileDebugChange
    , testCase "hamletFileDebug- features" caseHamletFileDebugFeatures
    , testCase "cassius" caseCassius
    , testCase "cassiusFile" caseCassiusFile
    , testCase "cassiusFileDebug" caseCassiusFileDebug
    , testCase "cassiusFileDebugChange" caseCassiusFileDebugChange
    , testCase "julius" caseJulius
    , testCase "coffee" caseCoffee
    , testCase "juliusFile" caseJuliusFile
    , testCase "coffeeFile" caseCoffeeFile
    , testCase "coffeeFileDebug" caseCoffeeFileDebug
    , testCase "juliusFileDebugChange" caseJuliusFileDebugChange
    , testCase "comments" caseComments
    , testCase "hamletFileDebug double foralls" caseDoubleForalls
    , testCase "cassius pseudo-class" casePseudo
    -- FIXME test is disabled , testCase "different binding names" caseDiffBindNames
    , testCase "blank line" caseBlankLine
    , testCase "leading spaces" caseLeadingSpaces
    , testCase "cassius all spaces" caseCassiusAllSpaces
    , testCase "cassius whitespace and colons" caseCassiusWhitespaceColons
    , testCase "cassius trailing comments" caseCassiusTrailingComments
    , testCase "hamlet angle bracket syntax" caseHamletAngleBrackets
    , testCase "hamlet module names" caseHamletModuleNames
    , testCase "cassius module names" caseCassiusModuleNames
    , testCase "julius module names" caseJuliusModuleNames
    , testCase "single dollar at and caret" caseSingleDollarAtCaret
    , testCase "dollar operator" caseDollarOperator
    , testCase "in a row" caseInARow
    , testCase "embedded slash" caseEmbeddedSlash
    , testCase "string literals" caseStringLiterals
    , testCase "embed json" caseEmbedJson
    , testCase "interpolated operators" caseOperators
    , testCase "HTML comments" caseHtmlComments
    , testCase "multi cassius" caseMultiCassius
    , testCase "nested maybes" caseNestedMaybes
    , testCase "lucius" caseLucius
    , testCase "conditional class" caseCondClass
    ]

data Url = Home | Sub SubUrl
data SubUrl = SubUrl
render :: Url -> [(String, String)] -> String
render Home qs = "url" ++ showParams qs
render (Sub SubUrl) qs = "suburl" ++ showParams qs

showParams :: [(String, String)] -> String
showParams [] = ""
showParams z =
    '?' : intercalate "&" (map go z)
  where
    go (x, y) = go' x ++ '=' : go' y
    go' = concatMap encodeUrlChar

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

data Arg url = Arg
    { getArg :: Arg url
    , var :: Html
    , url :: Url
    , embed :: Hamlet url
    , true :: Bool
    , false :: Bool
    , list :: [Arg url]
    , nothing :: Maybe String
    , just :: Maybe String
    , urlParams :: (Url, [(String, String)])
    }

theArg :: Arg url
theArg = Arg
    { getArg = theArg
    , var = string "<var>"
    , url = Home
    , embed = [$hamlet|embed|]
    , true = True
    , false = False
    , list = [theArg, theArg, theArg]
    , nothing = Nothing
    , just = Just "just"
    , urlParams = (Home, [("foo", "bar"), ("foo1", "bar1")])
    }

helper :: String -> Hamlet Url -> Assertion
helper res h = do
    let x = renderHamletText render h
    T.pack res @=? x

caseEmpty :: Assertion
caseEmpty = helper "" [$hamlet||]

caseStatic :: Assertion
caseStatic = helper "some static content" [$hamlet|some static content|]

caseTag :: Assertion
caseTag = do
    helper "<p class=\"foo\"><div id=\"bar\">baz</div></p>" [$hamlet|
<p .foo
  <#bar>baz
|]
    helper "<p class=\"foo.bar\"><div id=\"bar\">baz</div></p>" [$hamlet|
<p class=foo.bar
  <#bar>baz
|]

caseVar :: Assertion
caseVar = do
    helper "&lt;var&gt;" [$hamlet|#{var theArg}|]

caseVarChain :: Assertion
caseVarChain = do
    helper "&lt;var&gt;" [$hamlet|#{var (getArg (getArg (getArg theArg)))}|]

caseUrl :: Assertion
caseUrl = do
    helper (render Home []) [$hamlet|@{url theArg}|]

caseUrlChain :: Assertion
caseUrlChain = do
    helper (render Home []) [$hamlet|@{url (getArg (getArg (getArg theArg)))}|]

caseEmbed :: Assertion
caseEmbed = do
    helper "embed" [$hamlet|^{embed theArg}|]

caseEmbedChain :: Assertion
caseEmbedChain = do
    helper "embed" [$hamlet|^{embed (getArg (getArg (getArg theArg)))}|]

caseIf :: Assertion
caseIf = do
    helper "if" [$hamlet|
$if true theArg
    if
|]

caseIfChain :: Assertion
caseIfChain = do
    helper "if" [$hamlet|
$if true (getArg (getArg (getArg theArg)))
    if
|]

caseElse :: Assertion
caseElse = helper "else" [$hamlet|
$if false theArg
    if
$else
    else
|]

caseElseChain :: Assertion
caseElseChain = helper "else" [$hamlet|
$if false (getArg (getArg (getArg theArg)))
    if
$else
    else
|]

caseElseIf :: Assertion
caseElseIf = helper "elseif" [$hamlet|
$if false theArg
    if
$elseif true theArg
    elseif
$else
    else
|]

caseElseIfChain :: Assertion
caseElseIfChain = helper "elseif" [$hamlet|
$if false(getArg(getArg(getArg theArg)))
    if
$elseif true(getArg(getArg(getArg theArg)))
    elseif
$else
    else
|]

caseList :: Assertion
caseList = do
    helper "xxx" [$hamlet|
$forall _x <- (list theArg)
    x
|]

caseListChain :: Assertion
caseListChain = do
    helper "urlurlurl" [$hamlet|
$forall x <-  list(getArg(getArg(getArg(getArg(getArg (theArg))))))
    @{url x}
|]

caseScriptNotEmpty :: Assertion
caseScriptNotEmpty = helper "<script></script>" [$hamlet|<script|]

caseMetaEmpty :: Assertion
caseMetaEmpty = do
    helper "<meta>" [$hamlet|<meta|]
    helper "<meta/>" [$xhamlet|<meta|]
    helper "<meta>" [$hamlet|<meta>|]
    helper "<meta/>" [$xhamlet|<meta>|]

caseInputEmpty :: Assertion
caseInputEmpty = do
    helper "<input>" [$hamlet|<input|]
    helper "<input/>" [$xhamlet|<input|]
    helper "<input>" [$hamlet|<input>|]
    helper "<input/>" [$xhamlet|<input>|]

caseMultiClass :: Assertion
caseMultiClass = do
    helper "<div class=\"foo bar\"></div>" [$hamlet|<.foo.bar|]
    helper "<div class=\"foo bar\"></div>" [$hamlet|<.foo.bar>|]

caseAttribOrder :: Assertion
caseAttribOrder = do
    helper "<meta 1 2 3>" [$hamlet|<meta 1 2 3|]
    helper "<meta 1 2 3>" [$hamlet|<meta 1 2 3>|]

caseNothing :: Assertion
caseNothing = do
    helper "" [$hamlet|
$maybe _n <- nothing theArg
    nothing
|]
    helper "nothing" [$hamlet|
$maybe _n <- nothing theArg
    something
$nothing
    nothing
|]

caseNothingChain :: Assertion
caseNothingChain = helper "" [$hamlet|
$maybe n <- nothing(getArg(getArg(getArg theArg)))
    nothing #{n}
|]

caseJust :: Assertion
caseJust = helper "it's just" [$hamlet|
$maybe n <- just theArg
    it's #{n}
|]

caseJustChain :: Assertion
caseJustChain = helper "it's just" [$hamlet|
$maybe n <- just(getArg(getArg(getArg theArg)))
    it's #{n}
|]

caseConstructor :: Assertion
caseConstructor = do
    helper "url" [$hamlet|@{Home}|]
    helper "suburl" [$hamlet|@{Sub SubUrl}|]
    let text = "<raw text>"
    helper "<raw text>" [$hamlet|#{preEscapedString text}|]

caseUrlParams :: Assertion
caseUrlParams = do
    helper "url?foo=bar&amp;foo1=bar1" [$hamlet|@?{urlParams theArg}|]

caseEscape :: Assertion
caseEscape = do
    helper "#this is raw\n " [$hamlet|
\#this is raw
\
\ 
|]
    helper "$@^" [$hamlet|$@^|]

caseEmptyStatementList :: Assertion
caseEmptyStatementList = do
    helper "" [$hamlet|$if True|]
    helper "" [$hamlet|$maybe _x <- Nothing|]
    let emptyList = []
    helper "" [$hamlet|$forall _x <- emptyList|]

caseAttribCond :: Assertion
caseAttribCond = do
    helper "<select></select>" [$hamlet|<select :False:selected|]
    helper "<select selected></select>" [$hamlet|<select :True:selected|]
    helper "<meta var=\"foo:bar\">" [$hamlet|<meta var=foo:bar|]
    helper "<select selected></select>"
        [$hamlet|<select :true theArg:selected|]

    helper "<select></select>" [$hamlet|<select :False:selected>|]
    helper "<select selected></select>" [$hamlet|<select :True:selected>|]
    helper "<meta var=\"foo:bar\">" [$hamlet|<meta var=foo:bar>|]
    helper "<select selected></select>"
        [$hamlet|<select :true theArg:selected>|]

caseNonAscii :: Assertion
caseNonAscii = do
    helper "עִבְרִי" [$hamlet|עִבְרִי|]

caseMaybeFunction :: Assertion
caseMaybeFunction = do
    helper "url?foo=bar&amp;foo1=bar1" [$hamlet|
$maybe x <- Just urlParams
    @?{x theArg}
|]

caseTrailingDollarSign :: Assertion
caseTrailingDollarSign =
    helper "trailing space \ndollar sign #" [$hamlet|trailing space #
\
dollar sign #\
|]

caseNonLeadingPercent :: Assertion
caseNonLeadingPercent =
    helper "<span style=\"height:100%\">foo</span>" [$hamlet|
<span style=height:100%>foo
|]

caseQuotedAttribs :: Assertion
caseQuotedAttribs =
    helper "<input type=\"submit\" value=\"Submit response\">" [$hamlet|
<input type=submit value="Submit response"
|]

caseSpacedDerefs :: Assertion
caseSpacedDerefs = do
    helper "&lt;var&gt;" [$hamlet|#{var theArg}|]
    helper "<div class=\"&lt;var&gt;\"></div>" [$hamlet|<.#{var theArg}|]

caseAttribVars :: Assertion
caseAttribVars = do
    helper "<div id=\"&lt;var&gt;\"></div>" [$hamlet|<##{var theArg}|]
    helper "<div class=\"&lt;var&gt;\"></div>" [$hamlet|<.#{var theArg}|]
    helper "<div f=\"&lt;var&gt;\"></div>" [$hamlet|< f=#{var theArg}|]

    helper "<div id=\"&lt;var&gt;\"></div>" [$hamlet|<##{var theArg}>|]
    helper "<div class=\"&lt;var&gt;\"></div>" [$hamlet|<.#{var theArg}>|]
    helper "<div f=\"&lt;var&gt;\"></div>" [$hamlet|< f=#{var theArg}>|]

caseStringsAndHtml :: Assertion
caseStringsAndHtml = do
    let str = "<string>"
    let html = preEscapedString "<html>"
    helper "&lt;string&gt; <html>" [$hamlet|#{str} #{html}|]

caseNesting :: Assertion
caseNesting = do
    helper
      "<table><tbody><tr><td>1</td></tr><tr><td>2</td></tr></tbody></table>"
      [$hamlet|
<table
  <tbody
    $forall user <- users
        <tr
         <td>#{user}
|]
    helper
        (concat
          [ "<select id=\"foo\" name=\"foo\"><option selected></option>"
          , "<option value=\"true\">Yes</option>"
          , "<option value=\"false\">No</option>"
          , "</select>"
          ])
        [$hamlet|
<select #"#{name}" name=#{name}
    <option :isBoolBlank val:selected
    <option value=true :isBoolTrue val:selected>Yes
    <option value=false :isBoolFalse val:selected>No
|]
  where
    users = ["1", "2"]
    name = "foo"
    val = 5 :: Int
    isBoolBlank _ = True
    isBoolTrue _ = False
    isBoolFalse _ = False

caseTrailingSpace :: Assertion
caseTrailingSpace =
    helper "" [$hamlet|        |]

caseCurrency :: Assertion
caseCurrency =
    helper foo [$hamlet|#{foo}|]
  where
    foo = "eg: 5, $6, €7.01, £75"

caseExternal :: Assertion
caseExternal = do
    helper "foo<br>" $(hamletFile "external.hamlet")
    helper "foo<br/>" $(xhamletFile "external.hamlet")
  where
    foo = "foo"

caseParens :: Assertion
caseParens = do
    let plus = (++)
        x = "x"
        y = "y"
    helper "xy" [$hamlet|#{(plus x) y}|]
    helper "xxy" [$hamlet|#{plus (plus x x) y}|]
    let alist = ["1", "2", "3"]
    helper "123" [$hamlet|
$forall x <- (id id id id alist)
    #{x}
|]

caseHamletLiterals :: Assertion
caseHamletLiterals = do
    helper "123" [$hamlet|#{show 123}|]
    helper "123.456" [$hamlet|#{show 123.456}|]
    helper "-123" [$hamlet|#{show -123}|]
    helper "-123.456" [$hamlet|#{show -123.456}|]

helper' :: String -> Html -> Assertion
helper' res h = T.pack res @=? renderHtmlText h

caseHamlet' :: Assertion
caseHamlet' = do
    helper' "foo" [$hamlet|foo|]
    helper' "foo" [$xhamlet|foo|]
    helper "<br>" $ const $ [$hamlet|<br|]
    helper "<br/>" $ const $ [$xhamlet|<br|]

    -- new with generalized stuff
    helper' "foo" [$hamlet|foo|]
    helper' "foo" [$xhamlet|foo|]
    helper "<br>" $ const $ [$hamlet|<br|]
    helper "<br/>" $ const $ [$xhamlet|<br|]

caseHamletDebug :: Assertion
caseHamletDebug = do
    helper "<p>foo</p>\n<p>bar</p>\n" [$hamletDebug|
<p>foo
<p>bar
|]

caseHamletRT :: Assertion
caseHamletRT = do
    temp <- parseHamletRT defaultHamletSettings "#{var}"
    rt <- parseHamletRT defaultHamletSettings $
            unlines
                [ "#{baz(bar foo)} bin #"
                , "$forall l <- list"
                , "  #{l}"
                , "$maybe j <- just"
                , "  #{j}"
                , "$maybe n <- nothing"
                , "$nothing"
                , "  nothing"
                , "^{template}"
                , "@{url}"
                , "$if false"
                , "$elseif false"
                , "$elseif true"
                , "  a"
                , "$if false"
                , "$else"
                , "  b"
                , "@?{urlp}"
                ]
    let scope =
            [ (["foo", "bar", "baz"], HDHtml $ preEscapedString "foo<bar>baz")
            , (["list"], HDList
                [ [([], HDHtml $ string "1")]
                , [([], HDHtml $ string "2")]
                , [([], HDHtml $ string "3")]
                ])
            , (["just"], HDMaybe $ Just
                [ ([], HDHtml $ string "just")
                ])
            , (["nothing"], HDMaybe Nothing)
            , (["template"], HDTemplate temp)
            , (["var"], HDHtml $ string "var")
            , (["url"], HDUrl Home)
            , (["urlp"], HDUrlParams Home [("foo", "bar")])
            , (["true"], HDBool True)
            , (["false"], HDBool False)
            ]
    rend <- renderHamletRT rt scope render
    renderHtmlText rend @?=
        T.pack "foo<bar>baz bin 123justnothingvarurlaburl?foo=bar"

caseHamletFileDebugChange :: Assertion
caseHamletFileDebugChange = do
    let foo = "foo"
    writeFile "external-debug.hamlet" "#{foo} 1"
    helper "foo 1" $ $(hamletFileDebug "external-debug.hamlet")
    writeFile "external-debug.hamlet" "#{foo} 2"
    helper "foo 2" $ $(hamletFileDebug "external-debug.hamlet")
    writeFile "external-debug.hamlet" "#{foo} 1"

caseHamletFileDebugFeatures :: Assertion
caseHamletFileDebugFeatures = do
    let var = "var"
    let url = Home
    let urlp = (Home, [("foo", "bar")])
    let template = [$hamlet|template|]
    let true = True
    let just = Just "just"
        nothing = Nothing
    let list = words "1 2 3"
    let extra = "e"
    flip helper $(hamletFileDebug "external-debug2.hamlet") $ concat
        [ "var"
        , "var"
        , "url"
        , "url"
        , "suburl"
        , "url?foo=bar"
        , "template"
        , "truee"
        , "not truee"
        , "elseif truee"
        , "just"
        , "juste"
        , "nothinge"
        , "1e2e3e"
        ]

caseCassius :: Assertion
caseCassius = do
    let var = "var"
    let urlp = (Home, [("p", "q")])
    flip celper [$cassius|
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
    let urlp = (Home, [("p", "q")])
    flip celper $(cassiusFile "external1.cassius") $ concat
        [ "foo{background:#000;bar:baz;color:#F00}"
        , "bin{"
        , "background-image:url(url);"
        , "bar:bar;color:#7F6405;fvarx:someval;unicode-test:שלום;"
        , "urlp:url(url?p=q)}"
        ]

caseCassiusFileDebug :: Assertion
caseCassiusFileDebug = do
    let var = "var"
    let urlp = (Home, [("p", "q")])
    flip celper $(cassiusFileDebug "external1.cassius") $ concat
        [ "foo{background:#000;bar:baz;color:#F00}"
        , "bin{"
        , "background-image:url(url);"
        , "bar:bar;color:#7F6405;fvarx:someval;unicode-test:שלום;"
        , "urlp:url(url?p=q)}"
        ]

caseCassiusFileDebugChange :: Assertion
caseCassiusFileDebugChange = do
    let var = "var"
    writeFile "external2.cassius" "foo\n  #{var}: 1"
    celper "foo{var:1}" $(cassiusFileDebug "external2.cassius")
    writeFile "external2.cassius" "foo\n  #{var}: 2"
    celper "foo{var:2}" $(cassiusFileDebug "external2.cassius")
    writeFile "external2.cassius" "foo\n  #{var}: 1"

celper :: String -> Cassius Url -> Assertion
celper res h = do
    let x = renderCassius render h
    T.pack res @=? x

jmixin = [$julius|x = 5|]
cofmixin = [$coffee|x = 5|]

jelper :: String -> Julius Url -> Assertion
jelper res h = T.pack res @=? renderJulius render h

cofelper :: String -> Coffee Url -> Assertion
cofelper res h = do
  c <- renderCoffee render h
  T.pack res @=? c

varD = "123;"
urlp = (Home, [("p", "q")])

caseJulius :: Assertion
caseJulius = do
    flip jelper [$julius|'שלום';
#{varD}
'@{Home}';
'@?{urlp}';
^{jmixin};
|] $ renderedI "\r\n"

caseCoffee :: Assertion
caseCoffee = do
    flip cofelper [$coffee|'שלום'
%{varD}
'@{Home}'
'@?{urlp}'
^{cofmixin}
|] $ renderedI "\n"

renderedI nl = intercalate nl
        [ "'שלום';"
        , varD
        , "'url';"
        , "'url?p=q';"
        , "x = 5;"
        ] ++ nl

caseJuliusFile :: Assertion
caseJuliusFile = do
    let var = "123;"
    let renderedF = unlines
          [ "'שלום';"
          , var
          , "'url';"
          , "'url?p=q';"
          , "x = 5;"
          ]
    flip jelper $(juliusFile "external1.julius") renderedF

caseJuliusFileDebug :: Assertion
caseJuliusFileDebug = do
    let var = "123;"
    let renderedF = unlines
          [ "'שלום';"
          , var
          , "'url';"
          , "'url?p=q';"
          , "x = 5;"
          ]
    flip jelper $(juliusFileDebug "external1.julius") renderedF

caseCoffeeFile :: Assertion
caseCoffeeFile = do
    let var = "123;"
    let renderedF = unlines
          [ "'שלום';"
          , var
          , "'url';"
          , "'url?p=q';"
          , "x = 5;"
          ]
    flip cofelper $(coffeeFile "tests/external1.coffee") renderedF

caseCoffeeFileDebug :: Assertion
caseCoffeeFileDebug = do
    let var = "123;"
    let renderedF = unlines
          [ "'שלום';"
          , var
          , "'url';"
          , "'url?p=q';"
          , "x = 5;"
          ]
    flip cofelper $(coffeeFileDebug "tests/external1.coffee") renderedF

caseJuliusFileDebugChange :: Assertion
caseJuliusFileDebugChange = do
    let var = "somevar"
    writeFile "external2.julius" "var #{var} = 1;"
    jelper "var somevar = 1;" $(juliusFileDebug "external2.julius")
    writeFile "external2.julius" "var #{var} = 2;"
    jelper "var somevar = 2;" $(juliusFileDebug "external2.julius")
    writeFile "external2.julius" "var #{var} = 1;"

caseComments :: Assertion
caseComments = do
    -- FIXME reconsider Hamlet comment syntax?
    helper "" [$hamlet|$# this is a comment
$# another comment
$#a third one|]
    celper "" [$cassius|/* this is a comment */
/* another comment */
/*a third one*/|]

caseDoubleForalls :: Assertion
caseDoubleForalls = do
    let list = map show [1..2]
    helper "12" $(hamletFileDebug "double-foralls.hamlet")
instance Show Url where
    show _ = "FIXME remove this instance show Url"

casePseudo :: Assertion
casePseudo = do
    flip celper [$cassius|
a:visited
    color: blue
|] "a:visited{color:blue}"

caseDiffBindNames :: Assertion
caseDiffBindNames = do
    let list = words "1 2 3"
    -- FIXME helper "123123" $(hamletFileDebug "external-debug3.hamlet")
    error "test has been disabled"

caseBlankLine :: Assertion
caseBlankLine = do
    helper "<p>foo</p>" [$hamlet|
<p

    foo

|]
    celper "foo{bar:baz}" [$cassius|
foo

    bar: baz

|]

caseLeadingSpaces :: Assertion
caseLeadingSpaces =
    celper "foo{bar:baz}" [$cassius|
  foo
    bar: baz
|]

caseTrailingSpaces :: Assertion
caseTrailingSpaces = helper "" [$hamlet|
$if   True   
$elseif   False   
$else   
$maybe x <-   Nothing    
$nothing  
$forall   x     <-   empty       
|]
  where
    empty = []

caseCassiusAllSpaces :: Assertion
caseCassiusAllSpaces = do
    celper "h1{color:green }" [$cassius|
    h1
        color: green 
    |]

caseCassiusWhitespaceColons :: Assertion
caseCassiusWhitespaceColons = do
    celper "h1:hover{color:green ;font-family:sans-serif}" [$cassius|
    h1:hover
        color: green 
        font-family:sans-serif
    |]

caseCassiusTrailingComments :: Assertion
caseCassiusTrailingComments = do
    celper "h1:hover {color:green ;font-family:sans-serif}" [$cassius|
    h1:hover /* Please ignore this */
        color: green /* This is a comment. */
        /* Obviously this is ignored too. */
        font-family:sans-serif
    |]

caseHamletAngleBrackets :: Assertion
caseHamletAngleBrackets =
    helper "<p class=\"foo\" height=\"100\"><span id=\"bar\" width=\"50\">HELLO</span></p>"
        [$hamlet|
<p.foo height="100"
    <span #bar width=50>HELLO
|]

caseHamletModuleNames :: Assertion
caseHamletModuleNames =
    helper "oof oof 3.14 -5"
    [$hamlet|#{Data.List.reverse foo} #
#{L.reverse foo} #
#{show 3.14} #{show -5}|]
  where
    foo = "foo"

caseCassiusModuleNames :: Assertion
caseCassiusModuleNames =
    celper "sel{bar:oof oof 3.14 -5}"
    [$cassius|
sel
    bar: #{Data.List.reverse foo} #{L.reverse foo} #{show 3.14} #{show -5}
|]
  where
    foo = "foo"

caseJuliusModuleNames :: Assertion
caseJuliusModuleNames =
    jelper "oof oof 3.14 -5"
    [$julius|#{Data.List.reverse foo} #{L.reverse foo} #{show 3.14} #{show -5}|]
  where
    foo = "foo"

caseSingleDollarAtCaret :: Assertion
caseSingleDollarAtCaret = do
    helper "$@^" [$hamlet|$@^|]
    celper "sel{att:$@^}" [$cassius|
sel
    att: $@^
|]
    jelper "$@^" [$julius|$@^|]

    helper "#{@{^{" [$hamlet|#\{@\{^\{|]
    celper "sel{att:#{@{^{}" [$cassius|
sel
    att: #\{@\{^{
|]
    jelper "#{@{^{" [$julius|#\{@\{^\{|]

caseDollarOperator :: Assertion
caseDollarOperator = do
    let val = (1, (2, 3))
    helper "2" [$hamlet|#{ show $ fst $ snd val }|]
    helper "2" [$hamlet|#{ show $ fst $ snd $ val}|]

    celper "sel{att:2}" [$cassius|
sel
    att: #{ show $ fst $ snd val }
|]
    celper "sel{att:2}" [$cassius|
sel
    att: #{ show $ fst $ snd $ val}
|]

    jelper "2" [$julius|#{ show $ fst $ snd val }|]
    jelper "2" [$julius|#{ show $ fst $ snd $ val}|]

caseInARow :: Assertion
caseInARow = do
    helper "1" [$hamlet|#{ show $ const 1 2 }|]

caseEmbeddedSlash :: Assertion
caseEmbeddedSlash = do
    helper "///" [$hamlet|///|]
    celper "sel{att:///}" [$cassius|
sel
    att: ///
|]

caseStringLiterals :: Assertion
caseStringLiterals = do
    helper "string" [$hamlet|#{"string"}|]
    helper "string" [$hamlet|#{id "string"}|]
    helper "gnirts" [$hamlet|#{L.reverse $ id "string"}|]
    helper "str&quot;ing" [$hamlet|#{"str\"ing"}|]
    helper "str&lt;ing" [$hamlet|#{"str<ing"}|]

caseEmbedJson :: Assertion
caseEmbedJson = do
    let v = J.ValueObject $ Map.fromList
                [ ( T.pack "foo", J.ValueArray
                    [ J.ValueAtom $ J.AtomText $ T.pack "bar"
                    , J.ValueAtom $ J.AtomNumber 5
                    ])
                ]
    jelper "{\"foo\":[\"bar\",5.0]}" [$julius|#{v}|]

caseOperators = do
    helper "3" [$hamlet|#{show $ (+) 1 2}|]
    helper "6" [$hamlet|#{show $ sum $ (:) 1 ((:) 2 $ return 3)}|]

caseHtmlComments = do
    helper "<p>1</p><p>2</p>" [$hamlet|
<p>1
<!-- ignored comment -->
<p
    2
|]

caseMultiCassius :: Assertion
caseMultiCassius = do
    celper "foo{bar:baz;bar:bin}" [$cassius|
foo
    bar: baz
    bar: bin
|]

caseNestedMaybes :: Assertion
caseNestedMaybes = do
    let muser = Just "User" :: Maybe String
        mprof = Nothing :: Maybe Int
        m3 = Nothing :: Maybe String
    helper "justnothing" [$hamlet|
$maybe user <- muser
    $maybe profile <- mprof
        First two are Just
        $maybe desc <- m3
            \ and left us a description:
            <p>#{desc}
        $nothing
        and has left us no description.
    $nothing
        justnothing
$nothing
    <h1>No such Person exists.
   |]


caseLucius :: Assertion
caseLucius = do
    let var = "var"
    let urlp = (Home, [("p", "q")])
    flip celper [$lucius|
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

caseCondClass :: Assertion
caseCondClass = do
    helper "<p class=\"current\"></p>" [$hamlet|
<p :False:.ignored :True:.current
|]

    helper "<p class=\"1 3 2 4\"></p>" [$hamlet|
<p :True:.1 :True:class=2 :False:.a :False:class=b .3 class=4
|]

    helper "<p class=\"foo bar baz\"></p>" [$hamlet|
<p class=foo class=bar class=baz
|]
