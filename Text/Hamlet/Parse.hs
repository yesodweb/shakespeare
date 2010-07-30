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
    , xhtmlHamletSettings
    )
    where

import Control.Applicative ((<$>), Applicative (..))
import Control.Monad
import Control.Arrow
import Data.Data
import Data.List (intercalate)
import Text.ParserCombinators.Parsec hiding (Line)

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

data Deref = DerefLeaf Ident
           | DerefBranch Deref Deref
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
parseLines set s =
    case parse (many $ parseLine set) s s of
        Left e -> Error $ show e
        Right x -> Ok x

parseLine :: HamletSettings -> Parser (Int, Line)
parseLine set = do
    ss <- fmap sum $ many ((char ' ' >> return 1) <|>
                           (char '\t' >> return 4))
    x <- doctype <|>
         backslash <|>
         try controlIf <|>
         try controlElseIf <|>
         try (string "$else" >> eol >> return LineElse) <|>
         try controlMaybe <|>
         try (string "$nothing" >> eol >> return LineNothing) <|>
         try controlForall <|>
         tag <|>
         (do
            cs <- content InContent
            isEof <- (eof >> return True) <|> return False
            if null cs && ss == 0 && isEof
                then fail "End of Hamlet template"
                else return $ LineContent cs)
    return (ss, x)
  where
    eol' = (char '\n' >> return ()) <|> (string "\r\n" >> return ())
    eol = eof <|> eol'
    doctype = do
        string "!!!" >> eol
        return $ LineContent [ContentRaw $ hamletDoctype set ++ "\n"]
    backslash = do
        _ <- char '\\'
        (eol >> return (LineContent [ContentRaw "\n"]))
            <|> (LineContent <$> content InContent)
    controlIf = do
        _ <- string "$if"
        spaces
        x <- deref False
        eol
        return $ LineIf x
    controlElseIf = do
        _ <- string "$elseif"
        spaces
        x <- deref False
        eol
        return $ LineElseIf x
    controlMaybe = do
        _ <- string "$maybe"
        spaces
        x <- deref False
        spaces
        y <- ident
        eol
        return $ LineMaybe x y
    controlForall = do
        _ <- string "$forall"
        spaces
        x <- deref False
        spaces
        y <- ident
        eol
        return $ LineForall x y
    tag = do
        x <- tagName <|> tagIdent <|> tagClass
        xs <- many $ tagIdent <|> tagClass <|> tagAttrib
        c <- (eol >> return []) <|> (do
            _ <- many1 $ oneOf " \t"
            content InContent)
        let (tn, attr, classes) = tag' $ x : xs
        return $ LineTag tn attr c classes
    content cr = do
        x <- many $ content' cr
        case cr of
            InQuotes -> char '"' >> return ()
            NotInQuotes -> return ()
            InContent -> (char '$' >> eol) <|> eol
        return x
    content' cr = try contentDollar <|> contentAt <|> contentCarrot
                                <|> contentReg cr
    contentDollar = do
        _ <- char '$'
        (char '$' >> return (ContentRaw "$")) <|> (do
            s <- deref True
            _ <- char '$'
            return $ ContentVar s)
    contentAt = do
        _ <- char '@'
        (char '@' >> return (ContentRaw "@")) <|> (do
            x <- (char '?' >> return True) <|> return False
            s <- deref True
            _ <- char '@'
            return $ ContentUrl x s)
    contentCarrot = do
        _ <- char '^'
        (char '^' >> return (ContentRaw "^")) <|> (do
            s <- deref True
            _ <- char '^'
            return $ ContentEmbed s)
    contentReg InContent = ContentRaw <$> many1 (noneOf "$@^\r\n")
    contentReg NotInQuotes = ContentRaw <$> many1 (noneOf "$@^#.! \t\n\r")
    contentReg InQuotes =
        (do
            _ <- char '\\'
            ContentRaw . return <$> anyChar
        ) <|> (ContentRaw <$> many1 (noneOf "$@^\\\"\n\r"))
    tagName = do
        _ <- char '%'
        s <- many1 $ noneOf " \t.#!\r\n"
        return $ TagName s
    tagAttribValue = do
        cr <- (char '"' >> return InQuotes) <|> return NotInQuotes
        content cr
    tagIdent = char '#' >> TagIdent <$> tagAttribValue
    tagClass = char '.' >> TagClass <$> tagAttribValue
    tagAttrib = do
        _ <- char '!'
        cond <- (Just <$> tagAttribCond) <|> return Nothing
        s <- many1 $ noneOf " \t.!=\r\n"
        v <- (do
            _ <- char '='
            s' <- tagAttribValue
            return s') <|> return []
        return $ TagAttrib (cond, s, v)
    tagAttribCond = do
        _ <- char ':'
        d <- deref True
        _ <- char ':'
        return d
    tag' = foldr tag'' ("div", [], [])
    tag'' (TagName s) (_, y, z) = (s, y, z)
    tag'' (TagIdent s) (x, y, z) = (x, (Nothing, "id", s) : y, z)
    tag'' (TagClass s) (x, y, z) = (x, y, s : z)
    tag'' (TagAttrib s) (x, y, z) = (x, s : y, z)
    derefParens = between (char '(') (char ')') $ deref True
    derefSingle = derefParens <|> fmap DerefLeaf ident
    deref spaceAllowed = do
        let delim = if spaceAllowed
                        then (char '.' <|> (many1 (char ' ') >> return ' '))
                        else char '.'
        x <- derefSingle
        xs <- many $ delim >> derefSingle
        return $ foldr1 DerefBranch $ x : xs
    ident = Ident <$> many1 (alphaNum <|> char '_' <|> char '\'')

data TagPiece = TagName String
              | TagIdent [Content]
              | TagClass [Content]
              | TagAttrib (Maybe Deref, String, [Content])

data ContentRule = InQuotes | NotInQuotes | InContent

data Nest = Nest Line [Nest]

nestLines :: [(Int, Line)] -> [Nest]
nestLines [] = []
nestLines ((i, l):rest) =
    let (deeper, rest') = span (\(i', _) -> i' > i) rest
     in Nest l (nestLines deeper) : nestLines rest'

data Doc = DocForall Deref Ident [Doc]
         | DocCond [(Deref, [Doc])] (Maybe [Doc])
         | DocMaybe Deref Ident [Doc] (Maybe [Doc])
         | DocContent Content
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
                    DocContent $ ContentRaw $ "</" ++ tn ++ ">"
                _ -> DocContent $ ContentRaw ""
        seal = case closeStyle of
                 CloseInside -> DocContent $ ContentRaw "/>"
                 _ -> DocContent $ ContentRaw ">"
        start = DocContent $ ContentRaw $ "<" ++ tn
        attrs'' = concatMap attrToContent attrs'
    inside' <- nestToDoc set inside
    rest' <- nestToDoc set rest
    Ok $ start
       : attrs''
      ++ seal
       : map DocContent content
      ++ inside'
      ++ end
       : rest'
nestToDoc set (Nest (LineContent content) inside:rest) = do
    inside' <- nestToDoc set inside
    rest' <- nestToDoc set rest
    Ok $ map DocContent content ++ inside' ++ rest'
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
compressDoc (DocContent (ContentRaw ""):rest) = compressDoc rest
compressDoc ( DocContent (ContentRaw x)
            : DocContent (ContentRaw y)
            : rest
            ) = compressDoc $ (DocContent $ ContentRaw $ x ++ y) : rest
compressDoc (DocContent x:rest) = DocContent x : compressDoc rest

parseDoc :: HamletSettings -> String -> Result [Doc]
parseDoc set s = do
    ls <- parseLines set s
    let ns = nestLines ls
    ds <- nestToDoc set ns
    return $ compressDoc ds

attrToContent :: (Maybe Deref, String, [Content]) -> [Doc]
attrToContent (Just cond, k, v) =
    [DocCond [(cond, attrToContent (Nothing, k, v))] Nothing]
attrToContent (Nothing, k, []) = [DocContent $ ContentRaw $ ' ' : k]
attrToContent (Nothing, k, v) =
    DocContent (ContentRaw (' ' : k ++ "=\""))
  : map DocContent v
  ++ [DocContent $ ContentRaw "\""]

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

xhtmlHamletSettings :: HamletSettings
xhtmlHamletSettings =
    HamletSettings doctype True
  where
    doctype =
      "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" " ++
      "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"

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
