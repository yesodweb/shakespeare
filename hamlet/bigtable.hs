{-# LANGUAGE QuasiQuotes #-}

import Criterion.Main
import qualified Data.ByteString.Lazy.Char8 as L
import Text.Hamlet
import Data.ByteString.Char8 (pack)

bigTable :: ([Int], [Int]) -> L.ByteString
bigTable (rows, cols) = renderHamlet undefined $ [$hamlet|
%table
    $forall rows _row
        %tr
            $forall cols col
                %td $preEscapedString.show.col$
|]

main :: IO ()
main = defaultMain
    [ bench "bigTable" $ nf (L.length . bigTable) (rows, cols)]
  where
    rows = [1..1000] :: [Int]
    {-# NOINLINE rows #-}
    cols = [1..10] :: [Int]
    {-# NOINLINE cols #-}
