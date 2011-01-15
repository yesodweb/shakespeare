{-# LANGUAGE TemplateHaskell #-}
-- | General parsers, functions and datatypes for all three languages.
module Text.Shakespeare
    ( Deref (..)
    , parseDeref
    , derefToExp
    ) where

import Language.Haskell.TH.Syntax
import Data.Char (isUpper, isDigit)
import Text.ParserCombinators.Parsec
import Data.List (intercalate)

data Deref = DerefLeaf [String] String
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
    derefSingle = derefParens <|> ident
    deref = do
        let delim = many1 (char ' ') >> return ' '
        x <- derefSingle
        xs <- many $ delim >> derefSingle
        return $ foldr1 DerefBranch $ x : xs
    ident = do
        mods <- many modul
        func <- many1 (alphaNum <|> char '_' <|> char '\'')
        return $ DerefLeaf mods func
    modul = try $ do
        c <- upper
        cs <- many (alphaNum <|> char '_')
        _ <- char '.'
        return $ c : cs

derefToExp :: Deref -> Exp
derefToExp (DerefBranch x y) = derefToExp x `AppE` derefToExp y
derefToExp (DerefLeaf _ "") = error "Illegal empty ident"
derefToExp (DerefLeaf mods v@(s:_))
    | all isDigit v =
        if null mods
            then LitE $ IntegerL $ read v
            else error "Cannot have qualified numeric literals"
    | isUpper s = ConE $ mkName v
    | otherwise = VarE $ mkName v
  where
    name =
        if null mods
            then mkName v
            else Name (mkOccName v) (NameQ $ mkModName $ intercalate "." mods)
