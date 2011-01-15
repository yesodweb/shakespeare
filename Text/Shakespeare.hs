{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | General parsers, functions and datatypes for all three languages.
module Text.Shakespeare
    ( Deref
    , Ident (..)
    , Scope
    , parseDeref
    , derefToExp
    , flattenDeref
    ) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH (appE)
import Data.Char (isUpper)
import Text.ParserCombinators.Parsec
import Data.List (intercalate)
import Data.Ratio (numerator, denominator, (%))
import Data.Data (Data)
import Data.Typeable (Typeable)

newtype Ident = Ident String
    deriving (Show, Eq, Read, Data, Typeable, Lift)

type Scope = [(Ident, Exp)]

data Deref = DerefModulesIdent [String] Ident
           | DerefIdent Ident
           | DerefIntegral Integer
           | DerefRational Rational
           | DerefBranch Deref Deref
    deriving (Show, Eq, Read, Data, Typeable)

instance Lift Deref where
    lift (DerefModulesIdent v s) = do
        dl <- [|DerefModulesIdent|]
        v' <- lift v
        s' <- lift s
        return $ dl `AppE` v' `AppE` s'
    lift (DerefIdent s) = do
        dl <- [|DerefIdent|]
        s' <- lift s
        return $ dl `AppE` s'
    lift (DerefBranch x y) = do
        x' <- lift x
        y' <- lift y
        db <- [|DerefBranch|]
        return $ db `AppE` x' `AppE` y'
    lift (DerefIntegral i) = [|DerefIntegral|] `appE` lift i
    lift (DerefRational r) = do
        n <- lift $ numerator r
        d <- lift $ denominator r
        per <- [|(%)|]
        dr <- [|DerefRational|]
        return $ dr `AppE` (InfixE (Just n) per (Just d))

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
        let func' = Ident func
        return $
            if null mods
                then DerefIdent func'
                else DerefModulesIdent mods func'
    modul = try $ do
        c <- upper
        cs <- many (alphaNum <|> char '_')
        _ <- char '.'
        return $ c : cs

read' :: Read a => String -> String -> a
read' t s =
    case reads s of
        (x, _):_ -> x
        [] -> error $ t ++ " read failed: " ++ s

expType :: Ident -> Name -> Exp
expType (Ident (c:_)) = if isUpper c then ConE else VarE
expType (Ident "") = error "Bad Ident"

derefToExp :: Scope -> Deref -> Exp
derefToExp s (DerefBranch x y) = derefToExp s x `AppE` derefToExp s y
derefToExp _ (DerefModulesIdent mods i@(Ident s)) =
    expType i $ Name (mkOccName s) (NameQ $ mkModName $ intercalate "." mods)
derefToExp scope (DerefIdent i@(Ident s)) =
    case lookup i scope of
        Just e -> e
        Nothing -> expType i $ mkName s
derefToExp _ (DerefIntegral i) = LitE $ IntegerL i
derefToExp _ (DerefRational r) = LitE $ RationalL r

-- FIXME shouldn't we use something besides a list here?
flattenDeref :: Deref -> Maybe [String]
flattenDeref (DerefIdent (Ident x)) = Just [x]
flattenDeref (DerefBranch (DerefIdent (Ident x)) y) = do
    y' <- flattenDeref y
    Just $ y' ++ [x]
flattenDeref _ = Nothing
