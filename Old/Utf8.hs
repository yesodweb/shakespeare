module Old.Utf8
    ( charsToOctets
    , bsToChars
    , lbsToChars
    ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy.Char8 as L

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT

charsToOctets :: String -> String
charsToOctets = L.unpack . LT.encodeUtf8 . LT.pack

bsToChars :: S.ByteString -> String
bsToChars = T.unpack . T.decodeUtf8

lbsToChars :: L.ByteString -> String
lbsToChars = LT.unpack . LT.decodeUtf8
