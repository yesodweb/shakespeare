{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Quoter (quote, quoteFile, quoteFileReload) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote (QuasiQuoter (..))

#ifdef TEST_COFFEE
import Text.Coffee
import Text.Coffee (coffeeSettings)
import Text.Shakespeare (shakespeare)
#else
#  ifdef TEST_ROY
import Text.Roy
#  else
import Text.Julius
#  endif
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
#  ifdef TEST_ROY
quote = roy
quoteFile = royFile
quoteFileReload = royFileReload
#  else
quote = julius
quoteFile = juliusFile
quoteFileReload = juliusFileReload
#  endif
#endif
