import Test.Hspec
import ShakespeareJsTest (specs)

main :: IO ()
main = hspecX $ descriptions [specs]
