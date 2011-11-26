import  ShakespeareTextTest (specs)
import Test.Hspec

main :: IO ()
main = hspecX $ descriptions [specs]
