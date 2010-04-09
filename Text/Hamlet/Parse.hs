{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Text.Hamlet.Parse
    ( Result (..)
    , Deref (..)
    , Ident (..)
    , Content (..)
    , Doc (..)
    , parseDoc
#if TEST
    , testSuite
#endif
    )
    where

import Control.Applicative
import Control.Monad
import Control.Arrow
import Data.Data

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
#endif

data Result v = Error String | Ok v
    deriving (Show, Eq, Read, Data, Typeable)
instance Monad Result where
    return = Ok
    Error s >>= _ = Error s
    Ok v >>= f = f v
    fail = Error
instance Functor Result where
    fmap = liftM
instance Applicative Result where
    pure = return
    (<*>) = ap

newtype Deref = Deref [Ident]
    deriving (Show, Eq, Read, Data, Typeable)
newtype Ident = Ident String
    deriving (Show, Eq, Read, Data, Typeable)

data Content = ContentRaw String
             | ContentVar Deref
             | ContentUrl Deref
    deriving (Show, Eq, Read, Data, Typeable)

data Line = LineForall Deref Ident
          | LineIf Deref
          | LineElseIf Deref
          | LineElse
          | LineTag
            { _lineTagName :: String
            , _lineAttr :: [(String, [Content])]
            , _lineContent :: [Content]
            }
          | LineContent [Content]
    deriving (Eq, Show, Read)

parseLines :: String -> Result [(Int, Line)]
parseLines = mapM go . lines where
    go s = do
        let (spaces, s') = countSpaces 0 s
        l <- parseLine s'
        Ok (spaces, l)
    countSpaces i (' ':rest) = countSpaces (i + 1) rest
    countSpaces i ('\t':rest) = countSpaces (i + 4) rest
    countSpaces i x = (i, x)

parseLine :: String -> Result Line
parseLine ('$':'f':'o':'r':'a':'l':'l':' ':rest) =
    case words rest of
        [x, y] -> do
            x' <- parseDeref x
            y' <- parseIdent y
            return $ LineForall x' y'
        _ -> Error $ "Invalid forall: " ++ rest
parseLine ('$':'i':'f':' ':rest) = LineIf <$> parseDeref rest
parseLine ('$':'e':'l':'s':'e':'i':'f':' ':rest) = LineElseIf <$> parseDeref rest
parseLine "$else" = Ok LineElse
parseLine x@(c:_) | c `elem` "%#." = do
    let (begin, rest) = break (== ' ') x
        rest' = dropWhile (== ' ') rest
    (tn, attr) <- parseTag begin
    con <- parseContent rest'
    return $ LineTag tn attr con
parseLine s = LineContent <$> parseContent s

#if TEST
fooBar :: Deref
fooBar = Deref [Ident "foo", Ident "bar"]

caseParseLine :: Assertion
caseParseLine = do
    parseLine "$if foo.bar" @?= Ok (LineIf fooBar)
    parseLine "$elseif foo.bar" @?= Ok (LineElseIf fooBar)
    parseLine "$else" @?= Ok LineElse
    parseLine "%img!src=@foo.bar@"
        @?= Ok (LineTag "img" [("src", [ContentUrl fooBar])] [])
    parseLine ".$foo.bar$"
        @?= Ok (LineTag "div" [("class", [ContentVar fooBar])] [])
    parseLine "%span#foo.bar!baz=bin"
        @?= Ok (LineTag "span" [ ("baz", [ContentRaw "bin"])
                               , ("class", [ContentRaw "bar"])
                               , ("id", [ContentRaw "foo"])
                               ] [])
#endif

parseContent :: String -> Result [Content]
parseContent "" = Ok []
parseContent s =
    case break (flip elem "@$") s of
        (_, "") -> Ok [ContentRaw s]
        (a, delim:a') -> do
            b <-
                case break (== delim) a' of
                    ("", _:rest) -> do
                        rest' <- parseContent rest
                        return $ ContentRaw [delim] : rest'
                    (deref, _:rest) -> do
                        deref' <- parseDeref deref
                        let x = case delim of
                                    '$' -> ContentVar
                                    '@' -> ContentUrl
                        rest' <- parseContent rest
                        return $ x deref' : rest'
            case a of
                "" -> return b
                _ -> return $ ContentRaw a : b

#if TEST
caseParseContent :: Assertion
caseParseContent = do
    parseContent "" @?= Ok []
    parseContent "foo" @?= Ok [ContentRaw "foo"]
    parseContent "foo $bar$ baz" @?= Ok [ ContentRaw "foo "
                                        , ContentVar $ Deref [Ident "bar"]
                                        , ContentRaw " baz"
                                        ]
    parseContent "foo @bar@ baz" @?= Ok [ ContentRaw "foo "
                                        , ContentUrl $ Deref [Ident "bar"]
                                        , ContentRaw " baz"
                                        ]
    parseContent "@@" @?= Ok [ContentRaw "@"]
#endif

parseDeref :: String -> Result Deref
parseDeref "" = Error "Invalid empty deref"
parseDeref s = Deref <$> go s where
    go "" = return []
    go a = case break (== '.') a of
                (x, '.':y) -> do
                    x' <- parseIdent x
                    y' <- go y
                    return $ x' : y'
                (x, "") -> return <$> parseIdent x

#if TEST
caseParseDeref :: Assertion
caseParseDeref = do
    parseDeref "foo.bar.baz" @?= Ok (Deref [Ident "foo", Ident "bar", Ident "baz"])
#endif

parseIdent :: String -> Result Ident
parseIdent s
    | all (flip elem (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_'")) s
        = Ok $ Ident s
    | otherwise = Error $ "Invalid identifier: " ++ s

#if TEST
caseParseIdent :: Assertion
caseParseIdent = do
    parseIdent "foo" @?= Ok (Ident "foo")
#endif

parseTag :: String -> Result (String, [(String, [Content])])
parseTag s = do
    pieces <- takePieces s
    foldM go ("div", []) pieces
  where
    go (_, attrs) ('%':tn) = Ok (tn, attrs)
    go (tn, attrs) ('.':cl) = do
        con <- parseContent cl
        Ok (tn, ("class", con) : attrs)
    go (tn, attrs) ('#':cl) = do
        con <- parseContent cl
        Ok (tn, ("id", con) : attrs)
    go (tn, attrs) ('!':rest) = do
        let (name, val) = break (== '=') rest
        val' <-
            case val of
                ('=':rest') -> parseContent rest'
                _ -> Ok []
        Ok (tn, (name, val') : attrs)

takePieces :: String -> Result [String]
takePieces "" = Ok []
takePieces (a:s) = do
    (x, y) <- takePiece ((:) a) False False s
    y' <- takePieces y
    return $ x : y'
  where
    takePiece front False False "" = Ok (front "", "")
    takePiece _ _ _ "" = Error $ "Unterminated URL or var: " ++ s
    takePiece front False False (c:rest)
        | c `elem` "#.%!" = Ok (front "", c:rest)
    takePiece front x y (c:rest)
        | c == '$' = takePiece (front . (:) c) (not x) y rest
        | c == '@' = takePiece (front . (:) c) x (not y) rest
        | otherwise = takePiece (front . (:) c) x y rest

data Nest = Nest Line [Nest]

nestLines :: [(Int, Line)] -> [Nest]
nestLines [] = []
nestLines ((i, l):rest) =
    let (deeper, rest') = span (\(i', _) -> i' > i) rest
     in Nest l (nestLines deeper) : nestLines rest'

data Doc = DocForall Deref Ident [Doc]
         | DocCond [(Deref, [Doc])] (Maybe [Doc])
         | DocContent [Content]
    deriving (Show, Eq, Read, Data, Typeable)

nestToDoc :: [Nest] -> Result [Doc]
nestToDoc [] = Ok []
nestToDoc (Nest (LineForall d i) inside:rest) = do
    inside' <- nestToDoc inside
    rest' <- nestToDoc rest
    Ok $ DocForall d i inside' : rest'
nestToDoc (Nest (LineIf d) inside:rest) = do
    inside' <- nestToDoc inside
    (ifs, el, rest') <- parseConds ((:) (d, inside')) rest
    rest'' <- nestToDoc rest'
    Ok $ DocCond ifs el : rest''
nestToDoc (Nest (LineTag tn attrs content) inside:rest) = do
    let end = if closeTag tn || not (null content) || not (null inside)
                then [DocContent [ContentRaw $ "</" ++ tn ++ ">"]]
                else []
        start = ContentRaw $ "<" ++ tn
        attrs' = concatMap attrToContent attrs
    inside' <- nestToDoc inside
    rest' <- nestToDoc rest
    Ok $ DocContent (start : attrs' ++ ContentRaw ">" : content)
       : inside' ++ end ++ rest'
nestToDoc (Nest (LineContent content) inside:rest) = do
    inside' <- nestToDoc inside
    rest' <- nestToDoc rest
    Ok $ DocContent content : inside' ++ rest'
nestToDoc (Nest (LineElseIf _) _:_) = Error "Unexpected elseif"
nestToDoc (Nest LineElse _:_) = Error "Unexpected else"

compressDoc :: [Doc] -> [Doc]
compressDoc [] = []
compressDoc (DocForall d i doc:rest) =
    DocForall d i (compressDoc doc) : compressDoc rest
compressDoc (DocCond x y:rest) =
    DocCond (map (second compressDoc) x) (compressDoc `fmap` y)
    : compressDoc rest
compressDoc (DocContent x:DocContent y:rest) =
    compressDoc $ DocContent (x ++ y) : rest
compressDoc (DocContent x:rest) =
    DocContent (compressContent x) : compressDoc rest

compressContent :: [Content] -> [Content]
compressContent (ContentRaw "":rest) = compressContent rest
compressContent (ContentRaw x:ContentRaw y:rest) = compressContent $ ContentRaw (x ++ y) : rest
compressContent (x:rest) = x : compressContent rest
compressContent [] = []

parseDoc :: String -> Result [Doc]
parseDoc s = do
    ls <- parseLines s
    let ns = nestLines ls
    ds <- nestToDoc ns
    return $ compressDoc ds

attrToContent :: (String, [Content]) -> [Content]
attrToContent (k, []) = [ContentRaw $ ' ' : k]
attrToContent (k, v) = (ContentRaw $ ' ' : k ++ "=\"") : v ++ [ContentRaw "\""]

closeTag :: String -> Bool
closeTag "img" = False
closeTag "link" = False
closeTag "meta" = False
closeTag "br" = False
closeTag "hr" = False
closeTag _ = True

parseConds :: ([(Deref, [Doc])] -> [(Deref, [Doc])])
           -> [Nest]
           -> Result ([(Deref, [Doc])], Maybe [Doc], [Nest])
parseConds front (Nest LineElse inside:rest) = do
    inside' <- nestToDoc inside
    Ok $ (front [], Just inside', rest)
parseConds front (Nest (LineElseIf d) inside:rest) = do
    inside' <- nestToDoc inside
    parseConds (front . (:) (d, inside')) rest
parseConds front rest = Ok (front [], Nothing, rest)

#if TEST
---- Testing
testSuite :: Test
testSuite = testGroup "Text.Hamlet.Parse"
    [ testCase "parseLine" caseParseLine
    , testCase "parseContent" caseParseContent
    , testCase "parseDeref" caseParseDeref
    , testCase "parseIdent" caseParseIdent
    ]
#endif
