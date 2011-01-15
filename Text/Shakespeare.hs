{-# LANGUAGE TemplateHaskell #-}
-- | General parsers, functions and datatypes for all three languages.
module Text.Shakespeare
    ( Deref
    , parseDeref
    , derefToExp
    ) where

import Language.Haskell.TH.Syntax
import Data.Char (isUpper, isDigit)
import Text.ParserCombinators.Parsec
import Data.List (intercalate)

data Deref = DerefLeaf [String] String
           | DerefIntegral Integer
           | DerefRational Rational
           | DerefBranch Deref Deref
    deriving (Show, Eq)

instance Lift Deref where
    lift (DerefLeaf v s) = do
        dl <- [|DerefLeaf|]
        v' <- lift v
        return $ dl `AppE` v' `AppE` (LitE $ StringL s)
    lift (DerefBranch x y) = do
        x' <- lift x
        y' <- lift y
        db <- [|DerefBranch|]
        return $ db `AppE` x' `AppE` y'

parseDeref :: Parser Deref
parseDeref =
    deref
  where
    derefParens = between (char '(') (char ')') deref
    derefSingle = derefParens <|> numeric <|> ident
    deref = do
        let delim = many1 (char ' ') >> return ' '
        x <- derefSingle
        xs <- many $ delim >> derefSingle
        return $ foldr1 DerefBranch $ x : xs
    numeric = do
        n <- (char '-' >> return "-") <|> return ""
        x <- many1 digit
        y <- (char '.' >> fmap Just (many1 digit)) <|> return Nothing
        return $ case y of
            Nothing -> DerefIntegral $ read' "Integral" $ n ++ x
            Just z -> DerefRational $ toRational
                       (read' "Rational" $ n ++ x ++ '.' : z :: Double)
    ident = do
        mods <- many modul
        func <- many1 (alphaNum <|> char '_' <|> char '\'')
        return $ DerefLeaf mods func
    modul = try $ do
        c <- upper
        cs <- many (alphaNum <|> char '_')
        _ <- char '.'
        return $ c : cs

read' t s =
    case reads s of
        (x, _):_ -> x
        [] -> error $ t ++ " read failed: " ++ s

derefToExp :: Deref -> Exp
derefToExp (DerefBranch x y) = derefToExp x `AppE` derefToExp y
derefToExp (DerefLeaf _ "") = error "Illegal empty ident"
derefToExp (DerefLeaf mods v@(s:_))
    | isUpper s = ConE $ mkName v
    | otherwise = VarE $ mkName v
  where
    name =
        if null mods
            then mkName v
            else Name (mkOccName v) (NameQ $ mkModName $ intercalate "." mods)
derefToExp (DerefIntegral i) = LitE $ IntegerL i
derefToExp (DerefRational r) = LitE $ RationalL r
