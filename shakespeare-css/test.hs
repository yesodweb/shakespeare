import Test.Hspec
import ShakespeareCssTest (specs)

main :: IO ()
main = hspecX $ descriptions [specs]
