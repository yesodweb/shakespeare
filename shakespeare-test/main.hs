import Test.Hspec.Monadic
import qualified ShakespeareJsTest
import qualified ShakespeareBaseTest

main :: IO ()
main = hspec $ do
  ShakespeareJsTest.specs
  ShakespeareBaseTest.specs
