import HamletTest (specs)
import Test.Hspec

main :: IO ()
main = hspecX $ descriptions [specs]
