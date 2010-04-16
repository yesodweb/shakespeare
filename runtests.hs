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
    }

arg :: Monad m => Arg m url
arg = Arg
    { getArg = arg
    , getArgM = return arg
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
    }

helper :: String -> (Arg IO Url -> Hamlet Url IO ()) -> Assertion
helper res h = do
    x <- hamletToText render $ h arg
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
caseVar = helper "&lt;var&gt;" [$hamlet|$var$|]

caseVarMonad :: Assertion
caseVarMonad = helper "&lt;var&gt;" [$hamlet|$*mvar$|]

caseVarChain :: Assertion
caseVarChain = helper "&lt;var&gt;" [$hamlet|$getArg.*getArgM.getArg.var$|]

caseUrl :: Assertion
caseUrl = helper (render Home) [$hamlet|@url@|]

caseUrlMonad :: Assertion
caseUrlMonad = helper (render Home) [$hamlet|@*murl@|]

caseUrlChain :: Assertion
caseUrlChain = helper (render Home) [$hamlet|@getArg.*getArgM.getArg.url@|]

caseEmbed :: Assertion
caseEmbed = helper "embed" [$hamlet|^embed^|]

caseEmbedMonad :: Assertion
caseEmbedMonad = helper "embed" [$hamlet|^*membed^|]

caseEmbedChain :: Assertion
caseEmbedChain = helper "embed" [$hamlet|^getArg.*getArgM.getArg.embed^|]

caseIf :: Assertion
caseIf = helper "if" [$hamlet|
$if true
    if
|]

caseIfMonad :: Assertion
caseIfMonad = helper "if" [$hamlet|
$if *mtrue
    if
|]

caseIfChain :: Assertion
caseIfChain = helper "if" [$hamlet|
$if getArg.*getArgM.getArg.*mtrue
    if
|]

caseElse :: Assertion
caseElse = helper "else" [$hamlet|
$if false
    if
$else
    else
|]

caseElseMonad :: Assertion
caseElseMonad = helper "else" [$hamlet|
$if *mfalse
    if
$else
    else
|]

caseElseChain :: Assertion
caseElseChain = helper "else" [$hamlet|
$if getArg.*getArgM.getArg.*mfalse
    if
$else
    else
|]

caseElseIf :: Assertion
caseElseIf = helper "elseif" [$hamlet|
$if false
    if
$elseif true
    elseif
$else
    else
|]

caseElseIfMonad :: Assertion
caseElseIfMonad = helper "elseif" [$hamlet|
$if *mfalse
    if
$elseif *mtrue
    elseif
$else
    else
|]

caseElseIfChain :: Assertion
caseElseIfChain = helper "elseif" [$hamlet|
$if getArg.*getArgM.getArg.*mfalse
    if
$elseif getArg.*getArgM.getArg.*mtrue
    elseif
$else
    else
|]
