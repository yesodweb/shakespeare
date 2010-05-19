{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Text.Hamlet.Parse
    ( Result (..)
    , Deref (..)
    , Ident (..)
    , Content (..)
    , Doc (..)
    , parseDoc
    , HamletSettings (..)
    , defaultHamletSettings
#if TEST
    , testSuite
#endif
    )
    where

import Control.Applicative
import Control.Monad
import Control.Arrow
import Data.Data
import Data.List (intercalate)

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

newtype Deref = Deref [(Bool, Ident)] -- ^ is monadic, ident
    deriving (Show, Eq, Read, Data, Typeable)
newtype Ident = Ident String
    deriving (Show, Eq, Read, Data, Typeable)

data Content = ContentRaw String
             | ContentVar Deref
             | ContentUrl Bool Deref -- ^ bool: does it include params?
             | ContentEmbed Deref
    deriving (Show, Eq, Read, Data, Typeable)

data Line = LineForall Deref Ident
          | LineIf Deref
          | LineElseIf Deref
          | LineElse
          | LineMaybe Deref Ident
          | LineNothing
          | LineTag
            { _lineTagName :: String
            , _lineAttr :: [(Maybe Deref, String, [Content])]
            , _lineContent :: [Content]
            , _lineClasses :: [[Content]]
            }
          | LineContent [Content]
    deriving (Eq, Show, Read)

parseLines :: HamletSettings -> String -> Result [(Int, Line)]
parseLines set = mapM (go . killCarriage) . lines where
    go s = do
        let (spaces, s') = countSpaces 0 s
        l <- parseLine set s'
        Ok (spaces, l)
    countSpaces i (' ':rest) = countSpaces (i + 1) rest
    countSpaces i ('\t':rest) = countSpaces (i + 4) rest
    countSpaces i x = (i, x)
    killCarriage s
        | null s = s
        | last s == '\r' = init s
        | otherwise = s

parseLine :: HamletSettings -> String -> Result Line
parseLine set "!!!" =
    Ok $ LineContent [ContentRaw $ hamletDoctype set ++ "\n"]
parseLine _ "\\" = Ok $ LineContent [ContentRaw "\n"]
parseLine _ ('\\':s) = LineContent <$> parseContent s
parseLine _ ('$':'f':'o':'r':'a':'l':'l':' ':rest) =
    case words rest of
        [x, y] -> do
            x' <- parseDeref x
            (False, y') <- parseIdent' False y
            return $ LineForall x' y'
        _ -> Error $ "Invalid forall: " ++ rest
parseLine _ ('$':'i':'f':' ':rest) = LineIf <$> parseDeref rest
parseLine _ ('$':'e':'l':'s':'e':'i':'f':' ':rest) =
    LineElseIf <$> parseDeref rest
parseLine _ "$else" = Ok LineElse
parseLine _ ('$':'m':'a':'y':'b':'e':' ':rest) =
    case words rest of
        [x, y] -> do
            x' <- parseDeref x
            (False, y') <- parseIdent' False y
            return $ LineMaybe x' y'
        _ -> Error $ "Invalid maybe: " ++ rest
parseLine _ "$nothing" = Ok LineNothing
parseLine _ x@(c:_) | c `elem` "%#." = do
    let (begin, rest) = break (== ' ') x
        rest' = dropWhile (== ' ') rest
    (tn, attr, classes) <- parseTag begin
    con <- parseContent rest'
    return $ LineTag tn attr con classes
parseLine _ s = LineContent <$> parseContent s

#if TEST
fooBar :: Deref
fooBar = Deref [(False, Ident "bar"), (False, Ident "foo")]

caseParseLine :: Assertion
caseParseLine = do
    let parseLine' = parseLine defaultHamletSettings
    parseLine' "$if foo.bar" @?= Ok (LineIf fooBar)
    parseLine' "$elseif foo.bar" @?= Ok (LineElseIf fooBar)
    parseLine' "$else" @?= Ok LineElse
    parseLine' "%img!src=@foo.bar@"
        @?= Ok (LineTag "img" [(Nothing, "src", [ContentUrl False fooBar])] [] [])
    parseLine' "%img!src=@?foo.bar@"
        @?= Ok (LineTag "img" [(Nothing, "src", [ContentUrl True fooBar])] [] [])
    parseLine' ".$foo.bar$"
        @?= Ok (LineTag "div" [] [] [[ContentVar fooBar]])
    parseLine' "%span#foo.bar.bar2!baz=bin"
        @?= Ok (LineTag "span" [ (Nothing, "id", [ContentRaw "foo"])
                               , (Nothing, "baz", [ContentRaw "bin"])
                               ] []
                               [ [ContentRaw "bar"]
                               , [ContentRaw "bar2"]
                               ])
    parseLine' "\\#this is raw"
        @?= Ok (LineContent [ContentRaw "#this is raw"])
    parseLine' "\\"
        @?= Ok (LineContent [ContentRaw "\n"])
    parseLine' "%img!:baz:src=@foo.bar@"
        @?= Ok (LineTag "img"
                [(Just $ Deref [(False, Ident "baz")],
                  "src",
                  [ContentUrl False fooBar])] [] [])
#endif

parseContent :: String -> Result [Content]
parseContent "" = Ok []
parseContent s =
    case break (flip elem "@$^") s of
        (_, "") -> Ok [ContentRaw s]
        (a, delim:a') -> do
            b <-
                case break (== delim) a' of
                    ("", _:rest) -> do
                        rest' <- parseContent rest
                        return $ ContentRaw [delim] : rest'
                    (deref, _:rest) -> do
                        let (x, deref') = case delim of
                                    '$' -> (ContentVar, deref)
                                    '@' ->
                                        case deref of
                                            '?':y -> (ContentUrl True, y)
                                            _ -> (ContentUrl False, deref)
                                    '^' -> (ContentEmbed, deref)
                                    _ -> error $ "Invalid delim in parseContent: " ++ [delim]
                        deref'' <- parseDeref deref'
                        rest' <- parseContent rest
                        return $ x deref'' : rest'
                    (_, "") -> error $ "Missing ending delimiter in " ++ s
            case a of
                "" -> return b
                _ -> return $ ContentRaw a : b

#if TEST
caseParseContent :: Assertion
caseParseContent = do
    parseContent "" @?= Ok []
    parseContent "foo" @?= Ok [ContentRaw "foo"]
    parseContent "foo $bar$ baz" @?= Ok [ ContentRaw "foo "
                                        , ContentVar $ Deref [(False, Ident "bar")]
                                        , ContentRaw " baz"
                                        ]
    parseContent "foo @bar@ baz" @?=
        Ok [ ContentRaw "foo "
           , ContentUrl False (Deref [(False, Ident "bar")])
           , ContentRaw " baz"
           ]
    parseContent "foo @?bar@ baz" @?=
        Ok [ ContentRaw "foo "
           , ContentUrl True (Deref [(False, Ident "bar")])
           , ContentRaw " baz"
           ]
    parseContent "foo ^bar^ baz" @?= Ok [ ContentRaw "foo "
                                        , ContentEmbed $ Deref [(False, Ident "bar")]
                                        , ContentRaw " baz"
                                        ]
    parseContent "@@" @?= Ok [ContentRaw "@"]
    parseContent "^^" @?= Ok [ContentRaw "^"]
#endif

parseDeref :: String -> Result Deref
parseDeref "" = Error "Invalid empty deref"
parseDeref s = Deref . reverse <$> go s where
    go a = case break (== '.') a of
                (x, '.':y) -> do
                    x' <- parseIdent x
                    y' <- go y
                    return $ x' : y'
                (x, "") -> return <$> parseIdent x
                _ -> Error "Invalid branch in Text.Hamlet.Parse.parseDef"

#if TEST
caseParseDeref :: Assertion
caseParseDeref = do
    parseDeref "baz.*bar.foo" @?=
        Ok (Deref [ (False, Ident "foo")
                  , (True, Ident "bar")
                  , (False, Ident "baz")])
#endif

parseIdent :: String -> Result (Bool, Ident)
parseIdent ('*':s) = parseIdent' True s
parseIdent s = parseIdent' False s

parseIdent' :: Bool -> String -> Result (Bool, Ident)
parseIdent' _ "" = Error "Invalid empty ident"
parseIdent' b s
    | all (flip elem (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_'")) s
        = Ok (b, Ident s)
    | otherwise = Error $ "Invalid identifier: " ++ s

#if TEST
caseParseIdent :: Assertion
caseParseIdent = do
    parseIdent "foo" @?= Ok (False, Ident "foo")
    parseIdent "*foo" @?= Ok (True, Ident "foo")
#endif

parseTag :: String -> Result
                (String,
                 [(Maybe Deref, String, [Content])],
                 [[Content]])
parseTag s = do
    pieces <- takePieces s
    (a, b, c) <- foldM go ("div", id, id) pieces
    return (a, b [], c [])
  where
    go (_, attrs, classes) (_, '%':tn) = Ok (tn, attrs, classes)
    go (tn, attrs, classes) (_, '.':cl) = do
        con <- parseContent cl
        Ok (tn, attrs, classes . (:) con)
    go (tn, attrs, classes) (_, '#':cl) = do
        con <- parseContent cl
        Ok (tn, attrs . (:) (Nothing, "id", con), classes)
    go (tn, attrs, classes) (cond, '!':rest) = do
        let (name, val) = break (== '=') rest
        val' <- case val of
                    '=':x -> parseContent x
                    _ -> Ok []
        cond' <- case cond of
                    Nothing -> return Nothing
                    Just x -> Just <$> parseDeref x
        Ok (tn, attrs . (:) (cond', name, val'), classes)
    go _ _ = error "Invalid branch in Text.Hamlet.Parse.parseTag"

takePieces :: String -> Result [(Maybe String, String)]
takePieces "" = Ok []
takePieces (a:s) = do
    let (cond, s') =
            case (a, s) of
                ('!', ':':rest) ->
                    case break (== ':') rest of
                        (x, ':':y) -> (Just x, y)
                        _ -> (Nothing, s)
                _ -> (Nothing, s)
    (x, y) <- takePiece ((:) a) False False False s'
    y' <- takePieces y
    return $ (cond, x) : y'
  where
    takePiece front False False False "" = Ok (front "", "")
    takePiece _ _ _ _ "" = Error $ "Unterminated URL, var or embed: " ++ s
    takePiece front False False False (c:rest)
        | c `elem` "#.%!" = Ok (front "", c:rest)
    takePiece front x y z (c:rest)
        | c == '$' = takePiece (front . (:) c) (not x) y z rest
        | c == '@' = takePiece (front . (:) c) x (not y) z rest
        | c == '^' = takePiece (front . (:) c) x y (not z) rest
        | otherwise = takePiece (front . (:) c) x y z rest

data Nest = Nest Line [Nest]

nestLines :: [(Int, Line)] -> [Nest]
nestLines [] = []
nestLines ((i, l):rest) =
    let (deeper, rest') = span (\(i', _) -> i' > i) rest
     in Nest l (nestLines deeper) : nestLines rest'

data Doc = DocForall Deref Ident [Doc]
         | DocCond [(Deref, [Doc])] (Maybe [Doc])
         | DocMaybe Deref Ident [Doc] (Maybe [Doc])
         | DocContent [Content]
    deriving (Show, Eq, Read, Data, Typeable)

nestToDoc :: HamletSettings -> [Nest] -> Result [Doc]
nestToDoc _set [] = Ok []
nestToDoc set (Nest (LineForall d i) inside:rest) = do
    inside' <- nestToDoc set inside
    rest' <- nestToDoc set rest
    Ok $ DocForall d i inside' : rest'
nestToDoc set (Nest (LineIf d) inside:rest) = do
    inside' <- nestToDoc set inside
    (ifs, el, rest') <- parseConds set ((:) (d, inside')) rest
    rest'' <- nestToDoc set rest'
    Ok $ DocCond ifs el : rest''
nestToDoc set (Nest (LineMaybe d i) inside:rest) = do
    inside' <- nestToDoc set inside
    (nothing, rest') <-
        case rest of
            Nest LineNothing ninside:x -> do
                ninside' <- nestToDoc set ninside
                return (Just ninside', x)
            _ -> return (Nothing, rest)
    rest'' <- nestToDoc set rest'
    Ok $ DocMaybe d i inside' nothing : rest''
nestToDoc set (Nest (LineTag tn attrs content classes) inside:rest) = do
    let attrs' =
            case classes of
              [] -> attrs
              _ -> (Nothing, "class", intercalate [ContentRaw " "] classes)
                       : attrs
    let closeStyle =
            if not (null content) || not (null inside)
                then CloseSeparate
                else closeTag set tn
    let end = case closeStyle of
                CloseSeparate ->
                    DocContent [ContentRaw $ "</" ++ tn ++ ">"]
                _ -> DocContent []
        seal = case closeStyle of
                 CloseInside -> DocContent [ContentRaw "/>"]
                 _ -> DocContent [ContentRaw ">"]
        start = DocContent [ContentRaw $ "<" ++ tn]
        attrs'' = map attrToContent attrs'
    inside' <- nestToDoc set inside
    rest' <- nestToDoc set rest
    Ok $ start
       : attrs''
      ++ seal
       : DocContent content
       : inside'
      ++ end
       : rest'
nestToDoc set (Nest (LineContent content) inside:rest) = do
    inside' <- nestToDoc set inside
    rest' <- nestToDoc set rest
    Ok $ DocContent content : inside' ++ rest'
nestToDoc _set (Nest (LineElseIf _) _:_) = Error "Unexpected elseif"
nestToDoc _set (Nest LineElse _:_) = Error "Unexpected else"
nestToDoc _set (Nest LineNothing _:_) = Error "Unexpected nothing"

compressDoc :: [Doc] -> [Doc]
compressDoc [] = []
compressDoc (DocForall d i doc:rest) =
    DocForall d i (compressDoc doc) : compressDoc rest
compressDoc (DocMaybe d i doc mnothing:rest) =
    DocMaybe d i (compressDoc doc) (fmap compressDoc mnothing)
  : compressDoc rest
compressDoc (DocCond [(a, x)] Nothing:DocCond [(b, y)] Nothing:rest)
    | a == b = compressDoc $ DocCond [(a, x ++ y)] Nothing : rest
compressDoc (DocCond x y:rest) =
    DocCond (map (second compressDoc) x) (compressDoc `fmap` y)
    : compressDoc rest
compressDoc (DocContent x:DocContent y:rest) =
    compressDoc $ DocContent (x ++ y) : rest
compressDoc (DocContent x:rest) =
    DocContent (compressContent x) : compressDoc rest

compressContent :: [Content] -> [Content]
compressContent [] = []
compressContent (ContentRaw "":rest) = compressContent rest
compressContent (ContentRaw x:ContentRaw y:rest) =
    compressContent $ ContentRaw (x ++ y) : rest
compressContent (x:rest) = x : compressContent rest

parseDoc :: HamletSettings -> String -> Result [Doc]
parseDoc set s = do
    ls <- parseLines set s
    let ns = nestLines ls
    ds <- nestToDoc set ns
    return $ compressDoc ds

attrToContent :: (Maybe Deref, String, [Content]) -> Doc
attrToContent (Just cond, k, v) =
    DocCond [(cond, [attrToContent (Nothing, k, v)])] Nothing
attrToContent (Nothing, k, []) = DocContent [ContentRaw $ ' ' : k]
attrToContent (Nothing, k, v) = DocContent $
    ContentRaw (' ' : k ++ "=\"")
  : v ++ [ContentRaw "\""]

-- | Settings for parsing of a hamlet document.
data HamletSettings = HamletSettings
    {
      -- | The value to replace a \"!!!\" with. Do not include the trailing
      -- newline.
      hamletDoctype :: String
      -- | 'True' means to close empty tags (eg, img) with a trailing slash, ie
      -- XML-style empty tags. 'False' uses HTML-style.
    , hamletCloseEmpties :: Bool
    }

-- | Defaults settings: HTML5 doctype and HTML-style empty tags.
defaultHamletSettings :: HamletSettings
defaultHamletSettings = HamletSettings "<!DOCTYPE html>" False

data CloseStyle = NoClose | CloseInside | CloseSeparate

closeTag :: HamletSettings -> String -> CloseStyle
closeTag h s =
    if canBeEmpty s
        then CloseSeparate
        else (if hamletCloseEmpties h then CloseInside else NoClose)
  where
    canBeEmpty "img" = False
    canBeEmpty "link" = False
    canBeEmpty "meta" = False
    canBeEmpty "br" = False
    canBeEmpty "hr" = False
    canBeEmpty "input" = False
    canBeEmpty _ = True

parseConds :: HamletSettings
           -> ([(Deref, [Doc])] -> [(Deref, [Doc])])
           -> [Nest]
           -> Result ([(Deref, [Doc])], Maybe [Doc], [Nest])
parseConds set front (Nest LineElse inside:rest) = do
    inside' <- nestToDoc set inside
    Ok $ (front [], Just inside', rest)
parseConds set front (Nest (LineElseIf d) inside:rest) = do
    inside' <- nestToDoc set inside
    parseConds set (front . (:) (d, inside')) rest
parseConds _ front rest = Ok (front [], Nothing, rest)

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
