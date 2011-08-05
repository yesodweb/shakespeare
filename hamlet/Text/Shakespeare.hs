{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
-- | General parsers, functions and datatypes for all Shakespeare languages.
module Text.Shakespeare
    ( Deref (..)
    , Ident (..)
    , Scope
    , parseDeref
    , parseHash
    , parseVar
    , parseAt
    , parseUrl
    , parseCaret
    , parseUnder
    , parseInt
    , derefToExp
    , flattenDeref
    , readUtf8File
    ) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH (appE)
import Data.Char (isUpper)
import Text.ParserCombinators.Parsec
import Data.List (intercalate)
import Data.Ratio (Ratio, numerator, denominator, (%))
import Data.Data (Data)
import Data.Typeable (Typeable)
import qualified Data.Text.Lazy as TL
import qualified System.IO as SIO
import qualified Data.Text.Lazy.IO as TIO

newtype Ident = Ident String
    deriving (Show, Eq, Read, Data, Typeable)

type Scope = [(Ident, Exp)]

data Deref = DerefModulesIdent [String] Ident
           | DerefIdent Ident
           | DerefIntegral Integer
           | DerefRational Rational
           | DerefString String
           | DerefBranch Deref Deref
    deriving (Show, Eq, Read, Data, Typeable)

instance Lift Ident where
    lift (Ident s) = [|Ident|] `appE` lift s
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
        per <- [|(%) :: Int -> Int -> Ratio Int|]
        dr <- [|DerefRational|]
        return $ dr `AppE` (InfixE (Just n) per (Just d))
    lift (DerefString s) = [|DerefString|] `appE` lift s

parseDeref :: Parser Deref
parseDeref = do
    skipMany $ oneOf " \t"
    x <- derefSingle
    res <- deref' $ (:) x
    skipMany $ oneOf " \t"
    return res
  where
    delim = (many1 (char ' ') >> return())
            <|> lookAhead (char '(' >> return ())
    derefOp = try $ do
        _ <- char '('
        x <- many1 $ noneOf " \t\n\r()"
        _ <- char ')'
        return $ DerefIdent $ Ident x
    derefParens = between (char '(') (char ')') parseDeref
    derefSingle = derefOp <|> derefParens <|> numeric <|> strLit<|> ident
    deref' lhs =
        dollar <|> derefSingle'
               <|> return (foldl1 DerefBranch $ lhs [])
      where
        dollar = do
            _ <- try $ delim >> char '$'
            rhs <- parseDeref
            let lhs' = foldl1 DerefBranch $ lhs []
            return $ DerefBranch lhs' rhs
        derefSingle' = do
            x <- try $ delim >> derefSingle
            deref' $ lhs . (:) x
    numeric = do
        n <- (char '-' >> return "-") <|> return ""
        x <- many1 digit
        y <- (char '.' >> fmap Just (many1 digit)) <|> return Nothing
        return $ case y of
            Nothing -> DerefIntegral $ read' "Integral" $ n ++ x
            Just z -> DerefRational $ toRational
                       (read' "Rational" $ n ++ x ++ '.' : z :: Double)
    strLit = do
        _ <- char '"'
        chars <- many quotedChar
        _ <- char '"'
        return $ DerefString chars
    quotedChar = (char '\\' >> escapedChar) <|> noneOf "\""
    escapedChar =
        (char 'n' >> return '\n') <|>
        (char 'r' >> return '\r') <|>
        (char 'b' >> return '\b') <|>
        (char 't' >> return '\t') <|>
        (char '\\' >> return '\\') <|>
        (char '"' >> return '"') <|>
        (char '\'' >> return '\'')
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
expType (Ident (c:_)) = if isUpper c || c == ':' then ConE else VarE
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
derefToExp _ (DerefString s) = LitE $ StringL s

-- FIXME shouldn't we use something besides a list here?
flattenDeref :: Deref -> Maybe [String]
flattenDeref (DerefIdent (Ident x)) = Just [x]
flattenDeref (DerefBranch (DerefIdent (Ident x)) y) = do
    y' <- flattenDeref y
    Just $ y' ++ [x]
flattenDeref _ = Nothing

parseHash :: Parser (Either String Deref)
parseHash = parseVar '#'

parseVar :: Char -> Parser (Either String Deref)
parseVar c = do
    _ <- char c
    (char '\\' >> return (Left [c])) <|> (do
        _ <- char '{'
        deref <- parseDeref
        _ <- char '}'
        return $ Right deref) <|> (do
            -- Check for hash just before newline
            _ <- lookAhead (oneOf "\r\n" >> return ()) <|> eof
            return $ Left ""
            ) <|> return (Left [c])

parseAt :: Parser (Either String (Deref, Bool))
parseAt = parseUrl '@' '?'

parseUrl :: Char -> Char -> Parser (Either String (Deref, Bool))
parseUrl c d = do
    _ <- char c
    (char '\\' >> return (Left [c])) <|> (do
        x <- (char d >> return True) <|> return False
        (do
            _ <- char '{'
            deref <- parseDeref
            _ <- char '}'
            return $ Right (deref, x))
                <|> return (Left $ if x then [c, d] else [c]))

parseCaret :: Parser (Either String Deref)
parseCaret = parseInt '^'

parseInt :: Char -> Parser (Either String Deref)
parseInt c = do
    _ <- char c
    (char '\\' >> return (Left [c])) <|> (do
        _ <- char '{'
        deref <- parseDeref
        _ <- char '}'
        return $ Right deref) <|> return (Left [c])

parseUnder :: Parser (Either String Deref)
parseUnder = do
    _ <- char '_'
    (char '\\' >> return (Left "_")) <|> (do
        _ <- char '{'
        deref <- parseDeref
        _ <- char '}'
        return $ Right deref) <|> return (Left "_")

readUtf8File :: FilePath -> IO TL.Text
readUtf8File fp = do
    h <- SIO.openFile fp SIO.ReadMode
    SIO.hSetEncoding h SIO.utf8_bom
    TIO.hGetContents h
