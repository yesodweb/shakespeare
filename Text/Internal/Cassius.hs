{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Internal.Cassius (i2bMixin) where

import Data.Text.Lazy qualified as TL
import Text.IndentToBrace (i2b)

i2bMixin :: String -> String
i2bMixin s' =
    TL.unpack
        $ stripEnd "}"
        $ stripFront "mixin {"
        $ TL.strip
        $ TL.pack
        $ i2b
        $ unlines
        $ "mixin" : (map ("    " ++) $ lines s')
  where
    stripFront x y =
        case TL.stripPrefix x y of
            Nothing -> y
            Just z -> z
    stripEnd x y =
        case TL.stripSuffix x y of
            Nothing -> y
            Just z -> z