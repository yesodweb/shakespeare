{-# LANGUAGE QuasiQuotes #-}

import Criterion.Main
import qualified Data.ByteString.Lazy.Char8 as L
import Text.Hamlet
import Text.Hamlet.Monad
import Data.ByteString.Char8 (pack)

bigTable :: [[Int]] -> L.ByteString
bigTable t = hamletToByteString undefined $ [$hamlet|
%table
    $forall t row
        %tr
            $forall row col
                %td $Encoded.pack.show.col$
|]

main :: IO ()
main = defaultMain
    [ bench "bigTable" $ nf (L.length . bigTable) myTable ]
  where
    rows :: Int
    rows = 1000

    myTable :: [[Int]]
    myTable = replicate rows [1..10]
    {-# NOINLINE myTable #-}
