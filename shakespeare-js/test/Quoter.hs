{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Quoter (quote, quoteFile, quoteFileReload) where

import Language.Haskell.TH.Quote (QuasiQuoter)
import Language.Haskell.TH.Syntax
import Text.Coffee (coffeeSettings)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Text.Shakespeare (shakespeare)

#ifdef TEST_COFFEE
import Text.Coffee
#else
import Text.Julius
#endif

quote :: QuasiQuoter
quoteFile :: FilePath -> Q Exp
quoteFileReload :: FilePath -> Q Exp
#ifdef TEST_COFFEE
translate ('#':'{':rest) = translate $ '%':'{':translate rest
translate (c:other) = c:translate other
translate [] = []

quote = QuasiQuoter { quoteExp = \s -> do
    rs <- coffeeSettings
    quoteExp (shakespeare rs) (translate s)
    }

quoteFile = coffeeFile
quoteFileReload = coffeeFileReload
#else
quote = julius
quoteFile = juliusFile
quoteFileReload = juliusFileReload
#endif
