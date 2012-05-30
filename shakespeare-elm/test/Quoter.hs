{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Quoter (quote, quoteFile, quoteFileReload) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote (QuasiQuoter (..))

import Text.Elm

quote :: QuasiQuoter
quoteFile :: FilePath -> Q Exp
quoteFileReload :: FilePath -> Q Exp
quote = elm
quoteFile = elmFile
quoteFileReload = elmFileReload
