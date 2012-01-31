import Test.Hspec.Monadic
import qualified ShakespeareJsTest
import qualified ShakespeareBaseTest

main :: IO ()
main = hspecX $ do
  ShakespeareJsTest.specs
  ShakespeareBaseTest.specs
