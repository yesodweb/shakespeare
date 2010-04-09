import Test.Framework (defaultMain)

import qualified Text.Hamlet.Haml

main :: IO ()
main = defaultMain
    [ Text.Hamlet.Haml.testSuite
    ]
