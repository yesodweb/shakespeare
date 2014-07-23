{-# LANGUAGE OverloadedStrings #-}
module Text.IndentToBrace
    ( i2b
    ) where

import Control.Monad.Trans.Writer (execWriter, tell, Writer)
import Data.List (isInfixOf)
import qualified Data.Text as T

i2b :: String -> String
i2b = ($ [])
    . execWriter
    . mapM_ unnest
    . map addClosingCount
    . nest
    . map toL
    . stripComments
    . lines
    . filter (/= '\r')

stripComments :: [String] -> [String]
stripComments =
    map T.unpack . go False . map T.pack
  where
    go _ [] = []

    go False (l:ls) =
        let (before, after') = T.breakOn "/*" l
         in case T.stripPrefix "/*" after' of
                Nothing -> l : go False ls
                Just after ->
                    let (x:xs) = go True $ after : ls
                     in before `T.append` x : xs
    go True (l:ls) =
        let (_, after') = T.breakOn "*/" l
         in case T.stripPrefix "*/" after' of
                Nothing -> T.empty : go True ls
                Just after -> go False $ after : ls

data Line = Line
    { lineIndent  :: Int
    , lineContent :: String
    }
    deriving (Show, Eq)

data Nest = Nest Line Int [Nest]
          | Blank String
    deriving (Show, Eq)

isBlank :: Nest -> Bool
isBlank Blank{} = True
isBlank _ = False

addClosingCount :: Nest -> Nest
addClosingCount (Blank x) = Blank x
addClosingCount (Nest l c children) =
    Nest l c $ increment $ map addClosingCount children
  where
    increment
        | any (not . isBlank) children = increment'
        | otherwise = id

    increment' [] = error "should never happen"
    increment' (Blank x:rest) = Blank x : increment' rest
    increment' (n@(Nest l' c' children'):rest)
        | any (not . isBlank) rest = n : increment' rest
        | any (not . isBlank) children' = Nest l' c' (increment' children') : rest
        | otherwise = Nest l' (c' + 1) children' : rest

toL :: String -> Either String Line
toL s
    | null y = Left s
    | otherwise = Right $ Line (length x) y
  where
    (x, y) = span (== ' ') s

nest :: [Either String Line] -> [Nest]
nest [] = []
nest (Left x:rest) = Blank x : nest rest
nest (Right l:rest) =
    Nest l 0 (nest inside) : nest outside
  where
    (inside, outside) = span isNested rest
    isNested Left{} = True
    isNested (Right l2) = lineIndent l2 > lineIndent l

tell' :: String -> Writer (String -> String) ()
tell' s = tell (s ++)

unnest :: Nest -> Writer (String -> String) ()
unnest (Blank x) = do
    tell' x
    tell' "\n"
unnest (Nest l count inside) = do
    tell' $ replicate (lineIndent l) ' '
    tell' $ lineContent l
    tell' $
        case () of
            ()
                | not $ all isBlank inside -> " {"
                | ";" `isInfixOf` lineContent l -> ""
                | otherwise -> ";"
    tell' $ replicate count '}'
    tell' "\n"
    mapM_ unnest inside
