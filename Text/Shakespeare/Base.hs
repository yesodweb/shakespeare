{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
-- | General parsers, functions and datatypes for all Shakespeare languages.
module Text.Shakespeare.Base
    ( Deref (..)
    , Ident (..)
    , Scope
    , parseDeref
    , parseHash
    , parseVar
    , parseVarString
    , parseAt
    , parseUrl
    , parseUrlString
    , parseCaret
    , parseUnder
    , parseInt
    , parseIntString
    , derefToExp
    , flattenDeref
    , readUtf8File
    , readUtf8FileString
    , readFileQ
    , readFileRecompileQ
    ) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH (appE)
import Data.Char (isUpper, isSymbol, isPunctuation, isAscii)
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim (Parsec)
import Data.List (intercalate)
import Data.Ratio (Ratio, numerator, denominator, (%))
import Data.Data (Data)
import Data.Typeable (Typeable)
import qualified Data.Text.Lazy as TL
import qualified System.IO as SIO
import qualified Data.Text.Lazy.IO as TIO
import Control.Monad (when)

newtype Ident = Ident String
    deriving (Show, Eq, Read, Data, Typeable, Ord, Lift)

type Scope = [(Ident, Exp)]

data Deref = DerefModulesIdent [String] Ident
           | DerefIdent Ident
           | DerefIntegral Integer
           | DerefRational Rational
           | DerefString String
           | DerefBranch Deref Deref
           | DerefList [Deref]
           | DerefTuple [Deref]
    deriving (Show, Eq, Read, Data, Typeable, Ord, Lift)

derefParens, derefCurlyBrackets :: UserParser a Deref
derefParens        = between (char '(') (char ')') parseDeref
derefCurlyBrackets = between (char '{') (char '}') parseDeref

derefList, derefTuple :: UserParser a Deref
derefList = between (char '[') (char ']') (fmap DerefList $ sepBy parseDeref (char ','))
derefTuple = try $ do
  _ <- char '('
  x <- sepBy1 parseDeref (char ',')
  when (length x < 2) $ pzero
  _ <- char ')'
  return $ DerefTuple x

parseDeref :: UserParser a Deref
parseDeref = do
    skipMany (oneOf " \t")
    derefList <|> derefTuple <|> derefOther
  where
    derefOther = do
        x <- derefSingle
        derefInfix x <|> derefPrefix x
    delim = (many1 (char ' ') >> return())
            <|> lookAhead (oneOf "(\"" >> return ())
    derefOp = try $ do
            _ <- char '('
            x <- many1 $ noneOf " \t\n\r()"
            _ <- char ')'
            return $ DerefIdent $ Ident x

    -- See: http://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-160002.2
    isOperatorChar c
        | isAscii c = c `elem` "!#$%&*+./<=>?@\\^|-~:"
        | otherwise = isSymbol c || isPunctuation c

    derefPrefix x = do
        res <- deref' $ (:) x
        skipMany $ oneOf " \t"
        return res
    derefInfix x = try $ do
        _ <- delim
        xs <- many $ try $ derefSingle >>= \x' -> delim >> return x'
        op <- many1 (satisfy isOperatorChar) <?> "operator"
        -- special handling for $, which we don't deal with
        when (op == "$") $ fail "don't handle $"
        let op' = DerefIdent $ Ident op
        ys <- many1 $ try $ delim >> derefSingle
        skipMany $ oneOf " \t"
        return $ DerefBranch (DerefBranch op' $ foldl1 DerefBranch $ x : xs) (foldl1 DerefBranch ys)
    derefSingle = derefTuple <|> derefList <|> derefOp <|> derefParens <|> numeric <|> strLit <|> ident
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
        let cecs = [('n', '\n'),  ('r', '\r'), ('b', '\b'), ('t', '\t')
                   ,('\\', '\\'), ('"', '"'),  ('\'', '\'')]
        in choice [ char c >> return ec | (c, ec) <- cecs]
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
derefToExp s (DerefList ds) = ListE $ map (derefToExp s) ds
derefToExp s (DerefTuple ds) = TupE $
#if MIN_VERSION_template_haskell(2,16,0)
                               map Just $
#endif
                               map (derefToExp s) ds

-- FIXME shouldn't we use something besides a list here?
flattenDeref :: Deref -> Maybe [String]
flattenDeref (DerefIdent (Ident x)) = Just [x]
flattenDeref (DerefBranch (DerefIdent (Ident x)) y) = do
    y' <- flattenDeref y
    Just $ y' ++ [x]
flattenDeref _ = Nothing

parseHash :: UserParser a (Either String Deref)
parseHash = parseVar '#'

curlyBrackets :: UserParser a String
curlyBrackets = do
  _<- char '{'
  var <- many1 $ noneOf "}"
  _<- char '}'
  return $ ('{':var) ++ "}"


type UserParser a = Parsec String a

parseVar :: Char -> UserParser a (Either String Deref)
parseVar c = do
    _ <- char c
    (char '\\' >> return (Left [c])) <|> (do
        deref <- derefCurlyBrackets
        return $ Right deref) <|> (do
            -- Check for hash just before newline
            _ <- lookAhead (oneOf "\r\n" >> return ()) <|> eof
            return $ Left ""
            ) <|> return (Left [c])

parseAt :: UserParser a (Either String (Deref, Bool))
parseAt = parseUrl '@' '?'

parseUrl :: Char -> Char -> UserParser a (Either String (Deref, Bool))
parseUrl c d = do
    _ <- char c
    (char '\\' >> return (Left [c])) <|> (do
        x <- (char d >> return True) <|> return False
        (do
            deref <- derefCurlyBrackets
            return $ Right (deref, x))
                <|> return (Left $ if x then [c, d] else [c]))

parseInterpolatedString :: Char -> UserParser a (Either String String)
parseInterpolatedString c = do
    _ <- char c
    (char '\\' >> return (Left ['\\', c])) <|> (do
        bracketed <- curlyBrackets
        return $ Right (c:bracketed)) <|> return (Left [c])

parseVarString :: Char -> UserParser a (Either String String)
parseVarString = parseInterpolatedString

parseUrlString :: Char -> Char -> UserParser a (Either String String)
parseUrlString c d = do
    _ <- char c
    (char '\\' >> return (Left [c, '\\'])) <|> (do
        ds <- (char d >> return [d]) <|> return []
        (do bracketed <- curlyBrackets
            return $ Right (c:ds ++ bracketed))
                <|> return (Left (c:ds)))

parseIntString :: Char -> UserParser a (Either String String)
parseIntString = parseInterpolatedString

parseCaret :: UserParser a (Either String Deref)
parseCaret = parseInt '^'

parseInt :: Char -> UserParser a (Either String Deref)
parseInt c = do
    _ <- char c
    (try $ char '\\' >> char '{' >> return (Left [c, '{'])) <|> (do
        deref <- derefCurlyBrackets
        return $ Right deref) <|> return (Left [c])

parseUnder :: UserParser a (Either String Deref)
parseUnder = do
    _ <- char '_'
    (char '\\' >> return (Left "_")) <|> (do
        deref <- derefCurlyBrackets
        return $ Right deref) <|> return (Left "_")

-- | Read file's content as `String`, converting newlines
--
-- @since 2.0.19
readUtf8FileString :: FilePath -> IO String
readUtf8FileString fp = fmap TL.unpack $ readUtf8File fp

readUtf8File :: FilePath -> IO TL.Text
readUtf8File fp = do
    h <- SIO.openFile fp SIO.ReadMode
    SIO.hSetEncoding h SIO.utf8_bom
    ret <- TIO.hGetContents h
    return $
#ifdef WINDOWS
      TL.filter ('\r'/=) ret
#else
      ret
#endif

-- | Embed file's content, converting newlines
--
-- @since 2.0.19
readFileQ :: FilePath -> Q String
readFileQ fp = qRunIO (readUtf8FileString fp)

-- | Embed file's content, converting newlines
-- and track file via ghc dependencies, recompiling on changes
--
-- @since 2.0.19
readFileRecompileQ :: FilePath -> Q String
readFileRecompileQ fp = addDependentFile fp >> readFileQ fp
