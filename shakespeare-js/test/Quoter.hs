{-# LANGUAGE CPP #-}
module Quoter (quote, quoteFile, quoteFileReload) where

#ifdef TEST_COFFEE
import Text.Coffee
#else
import Text.Julius
#endif

#ifdef TEST_COFFEE
quote = coffee
quoteFile = coffeeFile
quoteFileReload = coffeeFileDebug
#else
quote = julius
quoteFile = juliusFile
quoteFileReload = juliusFileReload
#endif
