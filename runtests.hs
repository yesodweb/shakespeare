{-# LANGUAGE QuasiQuotes #-}
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import qualified Text.Hamlet.Parse
import Text.Hamlet
import Text.Hamlet.Monad (hamletToByteString)
import Data.ByteString.UTF8 (fromString)
import Data.ByteString.Lazy.UTF8 (toString)

main :: IO ()
main = defaultMain
    [ Text.Hamlet.Parse.testSuite
    , testSuite
    ]

testSuite :: Test
testSuite = testGroup "Text.Hamlet"
    [ testCase "empty" caseEmpty
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
    ]

data Url = Home | Sub SubUrl
data SubUrl = SubUrl
render :: Url -> String
render Home = "url"
render (Sub SubUrl) = "suburl"

data Arg url = Arg
    { getArg :: Arg url
    , var :: HtmlContent
    , url :: Url
    , embed :: (url -> String) -> Hamlet url
    , true :: Bool
    , false :: Bool
    , list :: [Arg url]
    , nothing :: Maybe HtmlContent
    , just :: Maybe HtmlContent
    , urlParams :: (Url, [(String, String)])
    }

theArg :: Arg url
theArg = Arg
    { getArg = theArg
    , var = Unencoded "<var>"
    , url = Home
    , embed = [$hamlet|embed|]
    , true = True
    , false = False
    , list = [theArg, theArg, theArg]
    , nothing = Nothing
    , just = Just $ Unencoded "just"
    , urlParams = (Home, [("foo", "bar"), ("foo1", "bar1")])
    }

helper :: String -> ((Url -> String) -> Hamlet Url) -> Assertion
helper res h = do
    let x = hamletToByteString render h
    res @=? toString x

caseEmpty :: Assertion
caseEmpty = helper "" [$hamlet||]

caseStatic :: Assertion
caseStatic = helper "some static content" [$hamlet|some static content|]

caseTag :: Assertion
caseTag = helper "<p class=\"foo\"><div id=\"bar\">baz</div></p>" [$hamlet|
%p.foo
 #bar baz|]

caseVar :: Assertion
caseVar = do
    helper "&lt;var&gt;" [$hamlet|$var.theArg$|]

caseVarChain :: Assertion
caseVarChain = do
    helper "&lt;var&gt;" [$hamlet|$var.getArg.getArg.getArg.theArg$|]

caseUrl :: Assertion
caseUrl = do
    helper (render Home) [$hamlet|@url.theArg@|]

caseUrlChain :: Assertion
caseUrlChain = do
    helper (render Home) [$hamlet|@url.getArg.getArg.getArg.theArg@|]

caseEmbed :: Assertion
caseEmbed = do
    helper "embed" [$hamlet|^embed.theArg^|]

caseEmbedChain :: Assertion
caseEmbedChain = do
    helper "embed" [$hamlet|^embed.getArg.getArg.getArg.theArg^|]

caseIf :: Assertion
caseIf = do
    helper "if" [$hamlet|
$if true.theArg
    if
|]

caseIfChain :: Assertion
caseIfChain = do
    helper "if" [$hamlet|
$if true.getArg.getArg.getArg.theArg
    if
|]

caseElse :: Assertion
caseElse = helper "else" [$hamlet|
$if false.theArg
    if
$else
    else
|]

caseElseChain :: Assertion
caseElseChain = helper "else" [$hamlet|
$if false.getArg.getArg.getArg.theArg
    if
$else
    else
|]

caseElseIf :: Assertion
caseElseIf = helper "elseif" [$hamlet|
$if false.theArg
    if
$elseif true.theArg
    elseif
$else
    else
|]

caseElseIfChain :: Assertion
caseElseIfChain = helper "elseif" [$hamlet|
$if false.getArg.getArg.getArg.theArg
    if
$elseif true.getArg.getArg.getArg.theArg
    elseif
$else
    else
|]

caseList :: Assertion
caseList = do
    helper "xxx" [$hamlet|
$forall list.theArg x
    x
|]

caseListChain :: Assertion
caseListChain = do
    helper "urlurlurl" [$hamlet|
$forall list.getArg.getArg.getArg.getArg.getArg.theArg x
    @url.x@
|]

caseScriptNotEmpty :: Assertion
caseScriptNotEmpty = helper "<script></script>" [$hamlet|%script|]

caseMetaEmpty :: Assertion
caseMetaEmpty = do
    helper "<meta>" [$hamlet|%meta|]
    helper "<meta/>" [$xhamlet|%meta|]

caseInputEmpty :: Assertion
caseInputEmpty = do
    helper "<input>" [$hamlet|%input|]
    helper "<input/>" [$xhamlet|%input|]

caseMultiClass :: Assertion
caseMultiClass = do
    helper "<div class=\"foo bar\"></div>" [$hamlet|.foo.bar|]

caseAttribOrder :: Assertion
caseAttribOrder = helper "<meta 1 2 3>" [$hamlet|%meta!1!2!3|]

caseNothing :: Assertion
caseNothing = do
    helper "" [$hamlet|
$maybe nothing.theArg n
    nothing
|]
    helper "nothing" [$hamlet|
$maybe nothing.theArg n
    something
$nothing
    nothing
|]

caseNothingChain :: Assertion
caseNothingChain = helper "" [$hamlet|
$maybe nothing.getArg.getArg.getArg.theArg n
    nothing $n$
|]

caseJust :: Assertion
caseJust = helper "it's just" [$hamlet|
$maybe just.theArg n
    it's $n$
|]

caseJustChain :: Assertion
caseJustChain = helper "it's just" [$hamlet|
$maybe just.getArg.getArg.getArg.theArg n
    it's $n$
|]

caseConstructor :: Assertion
caseConstructor = do
    helper "url" [$hamlet|@Home@|]
    helper "suburl" [$hamlet|@Sub.SubUrl@|]
    let text = "<raw text>"
    helper "<raw text>" [$hamlet|$Encoded.text$|]

caseUrlParams :: Assertion
caseUrlParams = do
    helper "url?foo=bar&amp;foo1=bar1" [$hamlet|@?urlParams.theArg@|]

caseEscape :: Assertion
caseEscape = do
    helper "#this is raw\n " [$hamlet|
\#this is raw
\
\ 
|]

caseEmptyStatementList :: Assertion
caseEmptyStatementList = do
    helper "" [$hamlet|$if True|]
    helper "" [$hamlet|$maybe Nothing x|]
    let emptyList = []
    helper "" [$hamlet|$forall emptyList x|]

caseAttribCond :: Assertion
caseAttribCond = do
    helper "<select></select>" [$hamlet|%select!:False:selected|]
    helper "<select selected></select>" [$hamlet|%select!:True:selected|]
    helper "<meta var=\"foo:bar\">" [$hamlet|%meta!var=foo:bar|]
    helper "<select selected></select>"
        [$hamlet|%select!:true.theArg:selected|]

caseNonAscii :: Assertion
caseNonAscii = do
    helper "עִבְרִי" [$hamlet|עִבְרִי|]

caseMaybeFunction :: Assertion
caseMaybeFunction = do
    helper "url?foo=bar&foo1=bar1" [$hamlet|
$maybe Just.urlParams x
    @?x.theArg@
|]
