{-# LANGUAGE QuasiQuotes #-}

import Criterion.Main
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Monoid (mconcat)
import Text.Hamlet
import Text.Hamlet.Monad
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy.Char8 (unpack)

bigTable :: [[Int]] -> String
bigTable t = unpack $ hamletToByteString undefined $ [$hamlet|
%table
    $forall t row
        %tr
            $forall row col
                %td $Encoded.pack.show.col$
|]

main = defaultMain
    [ bench "bigTable" $ nf bigTable myTable ]
  where
    rows :: Int
    rows = 1000

    myTable :: [[Int]]
    myTable = replicate rows [1..10]
    {-# NOINLINE myTable #-}
