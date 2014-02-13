import Test.Hspec
import qualified ShakespeareJsTest
import qualified ShakespeareBaseTest
import qualified HamletTest

main :: IO ()
main = hspec $ do
  ShakespeareJsTest.specs
  ShakespeareBaseTest.specs
  HamletTest.spec
