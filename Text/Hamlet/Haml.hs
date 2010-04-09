{-# LANGUAGE CPP #-}
module Text.Hamlet.Haml
    where

import Control.Applicative
import Control.Monad

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
#endif

data Result v = Error String | Ok v
    deriving (Eq, Show, Read)
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
    deriving (Eq, Show, Read)
newtype Ident = Ident String
    deriving (Eq, Show, Read)

data Content = ContentRaw String
             | ContentVar Deref
             | ContentUrl Deref
    deriving (Eq, Show, Read)

data Line = LineForall Deref Ident
          | LineIf Deref
          | LineElseIf Deref
          | LineElse
          | LineTag
            { lineTagName :: String
            , lineAttr :: [(String, [Content])]
            , lineContent :: [Content]
            }
          | LineContent [Content]
    deriving (Eq, Show, Read)

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
parseLine x@(c:_) | c `elem` "%#." = undefined
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
        @?= Ok (LineTag "img" [("img", [ContentUrl fooBar])] [])
    parseLine ".$foo.bar$"
        @?= Ok (LineTag "div" [("class", [ContentVar fooBar])] [])
    parseLine "%span#foo.bar!baz=bin"
        @?= Ok (LineTag "span" [ ("id", [ContentRaw "foo"])
                               , ("class", [ContentRaw "bar"])
                               , ("baz", [ContentRaw "bin"])
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

#if TEST
---- Testing
testSuite :: Test
testSuite = testGroup "Text.Hamlet.Haml"
    [ testCase "parseLine" caseParseLine
    , testCase "parseContent" caseParseContent
    , testCase "parseDeref" caseParseDeref
    , testCase "parseIdent" caseParseIdent
    ]
#endif
