{-# LANGUAGE QuasiQuotes #-}
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import qualified Text.Hamlet.Parse
import Text.Hamlet
import Text.Hamlet.Monad (hamletToText)
import Data.Text.Lazy (pack)

main :: IO ()
main = defaultMain
    [ Text.Hamlet.Parse.testSuite
    , testSuite
    ]

testSuite :: Test
testSuite = testGroup "Text.Hamlet"
    [ testCase "empty" caseEmpty
    ]

data Url = Home
render :: Url -> String
render Home = "/home/"

data Arg = Arg

arg :: Arg
arg = Arg

helper :: String -> (Arg -> Hamlet Url IO ()) -> Assertion
helper res h = do
    x <- hamletToText render $ h arg
    pack res @=? x

caseEmpty :: Assertion
caseEmpty = helper "" [$hamlet||]
