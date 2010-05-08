{-# LANGUAGE QuasiQuotes #-}
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import qualified Text.Hamlet.Parse
import Text.Hamlet
import Text.Hamlet.Monad (hamletToText)
import Data.Text (pack)
import Data.Text.Lazy (unpack)

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
    ]

data Url = Home
render :: Url -> String
render Home = "url"

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
    }

theArg :: Arg IO url
theArg = Arg
    { getArg = theArg
    , getArgM = return theArg
    , var = Unencoded $ pack "<var>"
    , mvar = return $ Unencoded $ pack "<var>"
    , url = Home
    , murl = return Home
    , embed = [$hamlet|embed|] ()
    , membed = return $ [$hamlet|embed|] ()
    , true = True
    , mtrue = return True
    , false = False
    , mfalse = return False
    , list = [theArg, theArg, theArg]
    , enum = fromList $ list theArg
    , nothing = Nothing
    , mnothing = return Nothing
    , just = Just $ Unencoded $ pack "just"
    , mjust = return $ Just $ Unencoded $ pack "just"
    }

helper :: String -> (Arg IO Url -> Hamlet Url IO ()) -> Assertion
helper res h = do
    x <- hamletToText render $ h theArg
    res @=? unpack x

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
    helper "&lt;var&gt;" [$hamlet|$theArg.var$|]
    helper "&lt;var&gt;" [$hamlet|$.var$|]

caseVarMonad :: Assertion
caseVarMonad = do
    helper "&lt;var&gt;" [$hamlet|$.*mvar$|]
    helper "&lt;var&gt;" [$hamlet|$theArg.*mvar$|]

caseVarChain :: Assertion
caseVarChain = helper "&lt;var&gt;" [$hamlet|$.getArg.*getArgM.getArg.var$|]

caseUrl :: Assertion
caseUrl = do
    helper (render Home) [$hamlet|@.url@|]
    helper (render Home) [$hamlet|@theArg.url@|]

caseUrlMonad :: Assertion
caseUrlMonad = do
    helper (render Home) [$hamlet|@.*murl@|]
    helper (render Home) [$hamlet|@theArg.*murl@|]

caseUrlChain :: Assertion
caseUrlChain = do
    helper (render Home) [$hamlet|@.getArg.*getArgM.getArg.url@|]
    helper (render Home) [$hamlet|@theArg.getArg.*getArgM.getArg.url@|]

caseEmbed :: Assertion
caseEmbed = do
    helper "embed" [$hamlet|^.embed^|]
    helper "embed" [$hamlet|^theArg.embed^|]

caseEmbedMonad :: Assertion
caseEmbedMonad = do
    helper "embed" [$hamlet|^.*membed^|]
    helper "embed" [$hamlet|^theArg.*membed^|]

caseEmbedChain :: Assertion
caseEmbedChain = do
    helper "embed" [$hamlet|^.getArg.*getArgM.getArg.embed^|]
    helper "embed" [$hamlet|^theArg.getArg.*getArgM.getArg.embed^|]

caseIf :: Assertion
caseIf = do
    helper "if" [$hamlet|
$if .true
    if
|]
    helper "if" [$hamlet|
$if theArg.true
    if
|]

caseIfMonad :: Assertion
caseIfMonad = do
    helper "if" [$hamlet|
$if .*mtrue
    if
|]
    helper "if" [$hamlet|
$if theArg.*mtrue
    if
|]

caseIfChain :: Assertion
caseIfChain = do
    helper "if" [$hamlet|
$if theArg.getArg.*getArgM.getArg.*mtrue
    if
|]
    helper "if" [$hamlet|
$if .getArg.*getArgM.getArg.*mtrue
    if
|]

caseElse :: Assertion
caseElse = helper "else" [$hamlet|
$if .false
    if
$else
    else
|]

caseElseMonad :: Assertion
caseElseMonad = helper "else" [$hamlet|
$if .*mfalse
    if
$else
    else
|]

caseElseChain :: Assertion
caseElseChain = helper "else" [$hamlet|
$if .getArg.*getArgM.getArg.*mfalse
    if
$else
    else
|]

caseElseIf :: Assertion
caseElseIf = helper "elseif" [$hamlet|
$if .false
    if
$elseif theArg.true
    elseif
$else
    else
|]

caseElseIfMonad :: Assertion
caseElseIfMonad = helper "elseif" [$hamlet|
$if .*mfalse
    if
$elseif .*mtrue
    elseif
$else
    else
|]

caseElseIfChain :: Assertion
caseElseIfChain = helper "elseif" [$hamlet|
$if theArg.getArg.*getArgM.getArg.*mfalse
    if
$elseif .getArg.*getArgM.getArg.*mtrue
    elseif
$else
    else
|]

caseList :: Assertion
caseList = do
    helper "xxx" [$hamlet|
$forall .list x
    x
|]
    helper "xxx" [$hamlet|
$forall theArg.list x
    x
|]

caseListChain :: Assertion
caseListChain = do
    helper "urlurlurl" [$hamlet|
$forall .getArg.*getArgM.getArg.getArg.*getArgM.list x
    @x.*murl@
|]
    helper "urlurlurl" [$hamlet|
$forall theArg.getArg.*getArgM.getArg.getArg.*getArgM.list x
    @x.*murl@
|]

caseEnum :: Assertion
caseEnum = do
    helper "xxx" [$hamlet|
$forall .*enum x
    x
|]
    helper "xxx" [$hamlet|
$forall theArg.*enum x
    x
|]

caseEnumChain :: Assertion
caseEnumChain = helper "urlurlurl" [$hamlet|
$forall .getArg.*getArgM.getArg.getArg.*getArgM.*enum x
    @x.*murl@
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
caseNothing = helper "" [$hamlet|
$maybe .nothing n
    nothing
|]

caseNothingMonad :: Assertion
caseNothingMonad = helper "" [$hamlet|
$maybe theArg.*mnothing n
    nothing $n$
|]

caseNothingChain :: Assertion
caseNothingChain = helper "" [$hamlet|
$maybe .getArg.*getArgM.getArg.*mnothing n
    nothing $n$
|]

caseJust :: Assertion
caseJust = helper "it's just" [$hamlet|
$maybe .just n
    it's $n$
|]

caseJustMonad :: Assertion
caseJustMonad = helper "it's just" [$hamlet|
$maybe .*mjust n
    it's $n$
|]

caseJustChain :: Assertion
caseJustChain = helper "it's just" [$hamlet|
$maybe .getArg.*getArgM.getArg.*mjust n
    it's $n$
|]
