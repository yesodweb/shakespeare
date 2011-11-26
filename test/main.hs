import qualified HamletTest
import qualified ShakespeareTextTest
import qualified ShakespeareJsTest
import qualified ShakespeareCssTest
import Test.Hspec

main :: IO ()
main = hspecX $ descriptions $ [
    HamletTest.specs
  , ShakespeareTextTest.specs
  , ShakespeareJsTest.specs
  , ShakespeareCssTest.specs
  ]
