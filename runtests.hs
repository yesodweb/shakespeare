import Test.Framework (defaultMain)

import qualified Text.Hamlet.Parse

main :: IO ()
main = defaultMain
    [ Text.Hamlet.Parse.testSuite
    ]
