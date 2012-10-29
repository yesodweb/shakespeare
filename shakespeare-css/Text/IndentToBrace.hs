module Text.IndentToBrace
    ( i2b
    ) where

import Control.Monad.Trans.Writer (execWriter, tell, Writer)
import Data.List (isPrefixOf, isInfixOf)

i2b :: String -> String
i2b = ($ [])
    . execWriter
    . mapM_ unnest
    . map addClosingCount
    . nest
    . map toL
    . lines
    . filter (/= '\r')

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
    | null y || "/*" `isPrefixOf` y = Left s
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
                | not $ all isBlank inside -> "{"
                | ";" `isInfixOf` lineContent l -> ""
                | otherwise -> ";"
    tell' $ replicate count '}'
    tell' "\n"
    mapM_ unnest inside
