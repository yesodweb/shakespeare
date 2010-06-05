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
    , testCase "var monad" caseVarMonad
    , testCase "var chain " caseVarChain
    , testCase "url" caseUrl
    , testCase "url monad" caseUrlMonad
    , testCase "url chain " caseUrlChain
    , testCase "embed" caseEmbed
    , testCase "embed monad" caseEmbedMonad
    , testCase "embed chain " caseEmbedChain
    , testCase "if" caseIf
    , testCase "if monad" caseIfMonad
    , testCase "if chain " caseIfChain
    , testCase "else" caseElse
    , testCase "else monad" caseElseMonad
    , testCase "else chain " caseElseChain
    , testCase "elseif" caseElseIf
    , testCase "elseif monad" caseElseIfMonad
    , testCase "elseif chain " caseElseIfChain
    , testCase "list" caseList
    , testCase "enum" caseEnum
    , testCase "list chain" caseListChain
    , testCase "enum chain" caseEnumChain
    , testCase "script not empty" caseScriptNotEmpty
    , testCase "meta empty" caseMetaEmpty
    , testCase "input empty" caseInputEmpty
    , testCase "multiple classes" caseMultiClass
    , testCase "attrib order" caseAttribOrder
    , testCase "nothing" caseNothing
    , testCase "nothing monad" caseNothingMonad
    , testCase "nothing chain " caseNothingChain
    , testCase "just" caseJust
    , testCase "just monad" caseJustMonad
    , testCase "just chain " caseJustChain
    , testCase "constructor" caseConstructor
    , testCase "url + params" caseUrlParams
    , testCase "url + params monad" caseUrlParamsMonad
    , testCase "escape" caseEscape
    , testCase "empty statement list" caseEmptyStatementList
    , testCase "attribute conditionals" caseAttribCond
    , testCase "non-ascii" caseNonAscii
    ]

data Url = Home | Sub SubUrl
data SubUrl = SubUrl
render :: Url -> String
render Home = "url"
render (Sub SubUrl) = "suburl"

data Arg m url = Arg
    { getArg :: Arg m url
    , getArgM :: m (Arg m url)
    , var :: HtmlContent
    , mvar :: m HtmlContent
    , url :: Url
    , murl :: m Url
    , embed :: Hamlet url m ()
    , membed :: m (Hamlet url m ())
    , true :: Bool
    , mtrue :: m Bool
    , false :: Bool
    , mfalse :: m Bool
    , list :: [Arg m url]
    , enum :: Enumerator (Arg m url) m
    , nothing :: Maybe HtmlContent
    , mnothing :: m (Maybe HtmlContent)
    , just :: Maybe HtmlContent
    , mjust :: m (Maybe HtmlContent)
    , urlParams :: (Url, [(String, String)])
    , murlParams :: m (Url, [(String, String)])
    }

theArg :: Arg IO url
theArg = Arg
    { getArg = theArg
    , getArgM = return theArg
    , var = Unencoded $ fromString "<var>"
    , mvar = return $ Unencoded $ fromString "<var>"
    , url = Home
    , murl = return Home
    , embed = [$hamlet|embed|]
    , membed = return [$hamlet|embed|]
    , true = True
    , mtrue = return True
    , false = False
    , mfalse = return False
    , list = [theArg, theArg, theArg]
    , enum = fromList $ list theArg
    , nothing = Nothing
    , mnothing = return Nothing
    , just = Just $ Unencoded $ fromString "just"
    , mjust = return $ Just $ Unencoded $ fromString "just"
    , urlParams = (Home, [("foo", "bar"), ("foo1", "bar1")])
    , murlParams = return $ urlParams theArg
    }

helper :: String -> Hamlet Url IO () -> Assertion
helper res h = do
    x <- hamletToByteString render h
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

caseVarMonad :: Assertion
caseVarMonad = do
    helper "&lt;var&gt;" [$hamlet|$*mvar.theArg$|]

caseVarChain :: Assertion
caseVarChain = do
    helper "&lt;var&gt;" [$hamlet|$var.getArg.*getArgM.getArg.theArg$|]

caseUrl :: Assertion
caseUrl = do
    helper (render Home) [$hamlet|@url.theArg@|]

caseUrlMonad :: Assertion
caseUrlMonad = do
    helper (render Home) [$hamlet|@*murl.theArg@|]

caseUrlChain :: Assertion
caseUrlChain = do
    helper (render Home) [$hamlet|@url.getArg.*getArgM.getArg.theArg@|]

caseEmbed :: Assertion
caseEmbed = do
    helper "embed" [$hamlet|^embed.theArg^|]

caseEmbedMonad :: Assertion
caseEmbedMonad = do
    helper "embed" [$hamlet|^*membed.theArg^|]

caseEmbedChain :: Assertion
caseEmbedChain = do
    helper "embed" [$hamlet|^embed.getArg.*getArgM.getArg.theArg^|]

caseIf :: Assertion
caseIf = do
    helper "if" [$hamlet|
$if true.theArg
    if
|]

caseIfMonad :: Assertion
caseIfMonad = do
    helper "if" [$hamlet|
$if *mtrue.theArg
    if
|]

caseIfChain :: Assertion
caseIfChain = do
    helper "if" [$hamlet|
$if *mtrue.getArg.*getArgM.getArg.theArg
    if
|]

caseElse :: Assertion
caseElse = helper "else" [$hamlet|
$if false.theArg
    if
$else
    else
|]

caseElseMonad :: Assertion
caseElseMonad = helper "else" [$hamlet|
$if *mfalse.theArg
    if
$else
    else
|]

caseElseChain :: Assertion
caseElseChain = helper "else" [$hamlet|
$if *mfalse.getArg.*getArgM.getArg.theArg
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

caseElseIfMonad :: Assertion
caseElseIfMonad = helper "elseif" [$hamlet|
$if *mfalse.theArg
    if
$elseif *mtrue.theArg
    elseif
$else
    else
|]

caseElseIfChain :: Assertion
caseElseIfChain = helper "elseif" [$hamlet|
$if *mfalse.getArg.*getArgM.getArg.theArg
    if
$elseif *mtrue.getArg.*getArgM.getArg.theArg
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
$forall list.*getArgM.getArg.getArg.*getArgM.getArg.theArg x
    @*murl.x@
|]

caseEnum :: Assertion
caseEnum = do
    helper "xxx" [$hamlet|
$forall *enum.theArg x
    x
|]
    helper "xxx" [$hamlet|
$forall *enum.theArg x
    x
|]

caseEnumChain :: Assertion
caseEnumChain = helper "urlurlurl" [$hamlet|
$forall *enum.*getArgM.getArg.getArg.*getArgM.getArg.theArg x
    @*murl.x@
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

caseNothingMonad :: Assertion
caseNothingMonad = helper "" [$hamlet|
$maybe *mnothing.theArg n
    nothing $n$
|]

caseNothingChain :: Assertion
caseNothingChain = helper "" [$hamlet|
$maybe *mnothing.getArg.*getArgM.getArg.theArg n
    nothing $n$
|]

caseJust :: Assertion
caseJust = helper "it's just" [$hamlet|
$maybe just.theArg n
    it's $n$
|]

caseJustMonad :: Assertion
caseJustMonad = helper "it's just" [$hamlet|
$maybe *mjust.theArg n
    it's $n$
|]

caseJustChain :: Assertion
caseJustChain = helper "it's just" [$hamlet|
$maybe *mjust.getArg.*getArgM.getArg.theArg n
    it's $n$
|]

caseConstructor :: Assertion
caseConstructor = do
    helper "url" [$hamlet|@Home@|]
    helper "suburl" [$hamlet|@Sub.SubUrl@|]
    let text = fromString "<raw text>"
    helper "<raw text>" [$hamlet|$Encoded.text$|]

caseUrlParams :: Assertion
caseUrlParams = do
    helper "url?foo=bar&foo1=bar1" [$hamlet|@?urlParams.theArg@|]

caseUrlParamsMonad :: Assertion
caseUrlParamsMonad = do
    helper "url?foo=bar&foo1=bar1" [$hamlet|@?*murlParams.theArg@|]

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
