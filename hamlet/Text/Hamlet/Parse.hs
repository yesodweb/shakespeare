{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
module Text.Hamlet.Parse
    ( Result (..)
    , Content (..)
    , Doc (..)
    , parseDoc
    , HamletSettings (..)
    , defaultHamletSettings
    , xhtmlHamletSettings
    , debugHamletSettings
    , CloseStyle (..)
    , Binding (..)
    )
    where

import Text.Shakespeare.Base
import Control.Applicative ((<$>), Applicative (..))
import Control.Monad
import Control.Arrow
import Data.Data
import Text.ParserCombinators.Parsec hiding (Line)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe, fromMaybe)

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

data Content = ContentRaw String
             | ContentVar Deref
             | ContentUrl Bool Deref -- ^ bool: does it include params?
             | ContentEmbed Deref
             | ContentMsg Deref
             | ContentAttrs Deref
    deriving (Show, Eq, Read, Data, Typeable)

data Line = LineForall Deref Binding
          | LineIf Deref
          | LineElseIf Deref
          | LineElse
          | LineWith [(Deref, Binding)]
          | LineMaybe Deref Binding
          | LineNothing
          | LineCase Deref
          | LineOf [Ident]
          | LineTag
            { _lineTagName :: String
            , _lineAttr :: [(Maybe Deref, String, Maybe [Content])]
            , _lineContent :: [Content]
            , _lineClasses :: [(Maybe Deref, [Content])]
            , _lineAttrs :: [Deref]
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
                           (char '\t' >> fail "Tabs are not allowed in Hamlet indentation"))
    x <- doctype <|>
         doctypeDollar <|>
         comment <|>
         htmlComment <|>
         backslash <|>
         controlIf <|>
         controlElseIf <|>
         (try (string "$else") >> spaceTabs >> eol >> return LineElse) <|>
         controlMaybe <|>
         (try (string "$nothing") >> spaceTabs >> eol >> return LineNothing) <|>
         controlForall <|>
         controlWith <|>
         controlCase <|>
         controlOf <|>
         angle <|>
         invalidDollar <|>
         (eol' >> return (LineContent [])) <|>
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
    spaceTabs = many $ oneOf " \t"
    doctype = do
        try $ string "!!!" >> eol
        return $ LineContent [ContentRaw $ hamletDoctype set ++ "\n"]
    doctypeDollar = do
        _ <- try $ string "$doctype "
        name <- many $ noneOf "\r\n"
        eol
        case lookup name doctypeNames of
            Nothing -> fail $ "Unknown doctype name: " ++ name
            Just val -> return $ LineContent [ContentRaw $ val ++ "\n"]

    invalidDollar = do
        _ <- char '$'
        fail "Received a command I did not understand. If you wanted a literal $, start the line with a backslash."
    comment = do
        _ <- try $ string "$#"
        _ <- many $ noneOf "\r\n"
        eol
        return $ LineContent []
    htmlComment = do
        _ <- try $ string "<!--"
        _ <- manyTill anyChar $ try $ string "-->"
        x <- many nonComments
        eol
        return $ LineContent [ContentRaw $ concat x] -- FIXME handle variables?
    nonComments = (many1 $ noneOf "\r\n<") <|> (do
        _ <- char '<'
        (do
            _ <- try $ string "!--"
            _ <- manyTill anyChar $ try $ string "-->"
            return "") <|> return "<")
    backslash = do
        _ <- char '\\'
        (eol >> return (LineContent [ContentRaw "\n"]))
            <|> (LineContent <$> content InContent)
    controlIf = do
        _ <- try $ string "$if"
        spaces
        x <- parseDeref
        _ <- spaceTabs
        eol
        return $ LineIf x
    controlElseIf = do
        _ <- try $ string "$elseif"
        spaces
        x <- parseDeref
        _ <- spaceTabs
        eol
        return $ LineElseIf x
    binding = do
        y <- identPattern
        spaces
        _ <- string "<-"
        spaces
        x <- parseDeref
        _ <- spaceTabs
        return (x,y)
    bindingSep = char ',' >> spaceTabs
    controlMaybe = do
        _ <- try $ string "$maybe"
        spaces
        (x,y) <- binding
        eol
        return $ LineMaybe x y
    controlForall = do
        _ <- try $ string "$forall"
        spaces
        (x,y) <- binding
        eol
        return $ LineForall x y
    controlWith = do
        _ <- try $ string "$with"
        spaces
        bindings <- (binding `sepBy` bindingSep) `endBy` eol
        return $ LineWith $ concat bindings -- concat because endBy returns a [[(Deref,Ident)]]
    controlCase = do
        _ <- try $ string "$case"
        spaces
        x <- parseDeref
        _ <- spaceTabs
        eol
        return $ LineCase x
    controlOf = do
        _   <- try $ string "$of"
        pat <- many1 $ try $ spaces >> ident
        _   <- spaceTabs
        eol
        return $ LineOf pat
    content cr = do
        x <- many $ content' cr
        case cr of
            InQuotes -> char '"' >> return ()
            NotInQuotes -> return ()
            NotInQuotesAttr -> return ()
            InContent -> eol
        return $ cc x
      where
        cc [] = []
        cc (ContentRaw a:ContentRaw b:c) = cc $ ContentRaw (a ++ b) : c
        cc (a:b) = a : cc b
    content' cr = contentHash <|> contentAt <|> contentCaret
                              <|> contentUnder
                              <|> contentReg cr
    contentHash = do
        x <- parseHash
        case x of
            Left str -> return $ ContentRaw str
            Right deref -> return $ ContentVar deref
    contentAt = do
        x <- parseAt
        return $ case x of
                    Left str -> ContentRaw str
                    Right (s, y) -> ContentUrl y s
    contentCaret = do
        x <- parseCaret
        case x of
            Left str -> return $ ContentRaw str
            Right deref -> return $ ContentEmbed deref
    contentUnder = do
        x <- parseUnder
        case x of
            Left str -> return $ ContentRaw str
            Right deref -> return $ ContentMsg deref
    contentReg InContent = (ContentRaw . return) <$> noneOf "#@^\r\n"
    contentReg NotInQuotes = (ContentRaw . return) <$> noneOf "@^#. \t\n\r>"
    contentReg NotInQuotesAttr = (ContentRaw . return) <$> noneOf "@^ \t\n\r>"
    contentReg InQuotes = (ContentRaw . return) <$> noneOf "#@^\\\"\n\r>"
    tagAttribValue notInQuotes = do
        cr <- (char '"' >> return InQuotes) <|> return notInQuotes
        content cr
    tagIdent = char '#' >> TagIdent <$> tagAttribValue NotInQuotes
    tagCond = do
        d <- between (char ':') (char ':') parseDeref
        tagClass (Just d) <|> tagAttrib (Just d)
    tagClass x = char '.' >> (TagClass . (,) x) <$> tagAttribValue NotInQuotes
    tagAttrib cond = do
        s <- many1 $ noneOf " \t=\r\n><"
        v <- (char '=' >> Just <$> tagAttribValue NotInQuotesAttr) <|> return Nothing
        return $ TagAttrib (cond, s, v)

    tagAttrs = do
        _ <- char '*'
        d <- between (char '{') (char '}') parseDeref
        return $ TagAttribs d

    tag' = foldr tag'' ("div", [], [], [])
    tag'' (TagName s) (_, y, z, as) = (s, y, z, as)
    tag'' (TagIdent s) (x, y, z, as) = (x, (Nothing, "id", Just s) : y, z, as)
    tag'' (TagClass s) (x, y, z, as) = (x, y, s : z, as)
    tag'' (TagAttrib s) (x, y, z, as) = (x, s : y, z, as)
    tag'' (TagAttribs s) (x, y, z, as) = (x, y, z, s : as)

    ident :: Parser Ident
    ident = Ident <$> many1 (alphaNum <|> char '_' <|> char '\'')

    identPattern :: Parser Binding
    identPattern = (between
        (char '(' >> spaces)
        (spaces >> char ')' >> spaces)
        (BindTuple <$> sepBy1 ident (spaces >> char ',' >> spaces))
        ) <|> (do
            i <- ident
            is <- many $ try $ (many $ char ' ') >> ident
            if null is
                then return $ BindVar i
                else return $ BindConstr i is
            )
    angle = do
        _ <- char '<'
        name' <- many  $ noneOf " \t.#\r\n!>"
        let name = if null name' then "div" else name'
        xs <- many $ try ((many $ oneOf " \t\r\n") >>
              (tagIdent <|> tagCond <|> tagClass Nothing <|> tagAttrs <|> tagAttrib Nothing))
        _ <- many $ oneOf " \t\r\n"
        _ <- char '>'
        c <- content InContent
        let (tn, attr, classes, attrsd) = tag' $ TagName name : xs
        if '/' `elem` tn
          then fail "A tag name may not contain a slash. Perhaps you have a closing tag in your HTML."
          else return $ LineTag tn attr c classes attrsd

data TagPiece = TagName String
              | TagIdent [Content]
              | TagClass (Maybe Deref, [Content])
              | TagAttrib (Maybe Deref, String, Maybe [Content])
              | TagAttribs Deref
    deriving Show

data ContentRule = InQuotes | NotInQuotes | NotInQuotesAttr | InContent

data Nest = Nest Line [Nest]

nestLines :: [(Int, Line)] -> [Nest]
nestLines [] = []
nestLines ((i, l):rest) =
    let (deeper, rest') = span (\(i', _) -> i' > i) rest
     in Nest l (nestLines deeper) : nestLines rest'

data Doc = DocForall Deref Binding [Doc]
         | DocWith [(Deref, Binding)] [Doc]
         | DocCond [(Deref, [Doc])] (Maybe [Doc])
         | DocMaybe Deref Binding [Doc] (Maybe [Doc])
         | DocCase Deref [([Ident], [Doc])]
         | DocContent Content
    deriving (Show, Eq, Read, Data, Typeable)

nestToDoc :: HamletSettings -> [Nest] -> Result [Doc]
nestToDoc _set [] = Ok []
nestToDoc set (Nest (LineForall d i) inside:rest) = do
    inside' <- nestToDoc set inside
    rest' <- nestToDoc set rest
    Ok $ DocForall d i inside' : rest'
nestToDoc set (Nest (LineWith dis) inside:rest) = do
    inside' <- nestToDoc set inside
    rest' <- nestToDoc set rest
    Ok $ DocWith dis inside' : rest'
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
nestToDoc set (Nest (LineCase d) inside:rest) = do
    let getOf (Nest (LineOf pat) insideC) = do
            insideC' <- nestToDoc set insideC
            Ok (pat, insideC')
        getOf _ = Error "Inside a $case there may only be $of.  Use '$of _' for a wildcard."
    cases <- mapM getOf inside
    rest' <- nestToDoc set rest
    Ok $ DocCase d cases : rest'
nestToDoc set (Nest (LineTag tn attrs content classes attrsD) inside:rest) = do
    let attrFix (x, y, z) = (x, y, [(Nothing, z)])
    let takeClass (a, "class", b) = Just (a, fromMaybe [] b)
        takeClass _ = Nothing
    let clazzes = classes ++ mapMaybe takeClass attrs
    let notClass (_, x, _) = x /= "class"
    let noclass = filter notClass attrs
    let attrs' =
            case clazzes of
              [] -> map attrFix noclass
              _ -> (Nothing, "class", map (second Just) clazzes)
                       : map attrFix noclass
    let closeStyle =
            if not (null content) || not (null inside)
                then CloseSeparate
                else hamletCloseStyle set tn
    let end = case closeStyle of
                CloseSeparate ->
                    DocContent $ ContentRaw $ "</" ++ tn ++ ">"
                _ -> DocContent $ ContentRaw ""
        seal = case closeStyle of
                 CloseInside -> DocContent $ ContentRaw "/>"
                 _ -> DocContent $ ContentRaw ">"
        start = DocContent $ ContentRaw $ "<" ++ tn
        attrs'' = concatMap attrToContent attrs'
        newline' = DocContent $ ContentRaw
                 $ if hamletCloseNewline set then "\n" else ""
    inside' <- nestToDoc set inside
    rest' <- nestToDoc set rest
    Ok $ start
       : attrs''
      ++ map (DocContent . ContentAttrs) attrsD
      ++ seal
       : map DocContent content
      ++ inside'
      ++ end
       : newline'
       : rest'
nestToDoc set (Nest (LineContent content) inside:rest) = do
    inside' <- nestToDoc set inside
    rest' <- nestToDoc set rest
    Ok $ map DocContent content ++ inside' ++ rest'
nestToDoc _set (Nest (LineElseIf _) _:_) = Error "Unexpected elseif"
nestToDoc _set (Nest LineElse _:_) = Error "Unexpected else"
nestToDoc _set (Nest LineNothing _:_) = Error "Unexpected nothing"
nestToDoc _set (Nest (LineOf _) _:_) = Error "Unexpected 'of' (did you forget a $case?)"

compressDoc :: [Doc] -> [Doc]
compressDoc [] = []
compressDoc (DocForall d i doc:rest) =
    DocForall d i (compressDoc doc) : compressDoc rest
compressDoc (DocWith dis doc:rest) =
    DocWith dis (compressDoc doc) : compressDoc rest
compressDoc (DocMaybe d i doc mnothing:rest) =
    DocMaybe d i (compressDoc doc) (fmap compressDoc mnothing)
  : compressDoc rest
compressDoc (DocCond [(a, x)] Nothing:DocCond [(b, y)] Nothing:rest)
    | a == b = compressDoc $ DocCond [(a, x ++ y)] Nothing : rest
compressDoc (DocCond x y:rest) =
    DocCond (map (second compressDoc) x) (compressDoc `fmap` y)
    : compressDoc rest
compressDoc (DocCase d cs:rest) =
    DocCase d (map (second compressDoc) cs) : compressDoc rest
compressDoc (DocContent (ContentRaw ""):rest) = compressDoc rest
compressDoc ( DocContent (ContentRaw x)
            : DocContent (ContentRaw y)
            : rest
            ) = compressDoc $ (DocContent $ ContentRaw $ x ++ y) : rest
compressDoc (DocContent x:rest) = DocContent x : compressDoc rest

parseDoc :: HamletSettings -> String -> Result [Doc]
parseDoc set s = do
    ls <- parseLines set s
    let notEmpty (_, LineContent []) = False
        notEmpty _ = True
    let ns = nestLines $ filter notEmpty ls
    ds <- nestToDoc set ns
    return $ compressDoc ds

attrToContent :: (Maybe Deref, String, [(Maybe Deref, Maybe [Content])]) -> [Doc]
attrToContent (Just cond, k, v) =
    [DocCond [(cond, attrToContent (Nothing, k, v))] Nothing]
attrToContent (Nothing, k, []) = [DocContent $ ContentRaw $ ' ' : k]
attrToContent (Nothing, k, [(Nothing, Nothing)]) = [DocContent $ ContentRaw $ ' ' : k]
attrToContent (Nothing, k, [(Nothing, Just v)]) =
    DocContent (ContentRaw (' ' : k ++ "=\""))
  : map DocContent v
  ++ [DocContent $ ContentRaw "\""]
attrToContent (Nothing, k, v) = -- only for class
      DocContent (ContentRaw (' ' : k ++ "=\""))
    : concatMap go (init v)
    ++ go' (last v)
    ++ [DocContent $ ContentRaw "\""]
  where
    go (Nothing, x) = map DocContent (fromMaybe [] x) ++ [DocContent $ ContentRaw " "]
    go (Just b, x) =
        [ DocCond
            [(b, map DocContent (fromMaybe [] x) ++ [DocContent $ ContentRaw " "])]
            Nothing
        ]
    go' (Nothing, x) = maybe [] (map DocContent) x
    go' (Just b, x) =
        [ DocCond
            [(b, maybe [] (map DocContent) x)]
            Nothing
        ]

-- | Settings for parsing of a hamlet document.
data HamletSettings = HamletSettings
    {
      -- | The value to replace a \"!!!\" with. Do not include the trailing
      -- newline.
      hamletDoctype :: String
      -- | Should we put a newline after closing a tag? Mostly useful for debug
      -- output.
    , hamletCloseNewline :: Bool
      -- | How a tag should be closed. Use this to switch between HTML, XHTML
      -- or even XML output.
    , hamletCloseStyle :: String -> CloseStyle
    }

htmlEmptyTags :: Set String
htmlEmptyTags = Set.fromAscList
    [ "area"
    , "base"
    , "basefont"
    , "br"
    , "col"
    , "frame"
    , "hr"
    , "img"
    , "input"
    , "isindex"
    , "link"
    , "meta"
    , "param"
    ]

-- | Defaults settings: HTML5 doctype and HTML-style empty tags.
defaultHamletSettings :: HamletSettings
defaultHamletSettings = HamletSettings "<!DOCTYPE html>" False htmlCloseStyle

xhtmlHamletSettings :: HamletSettings
xhtmlHamletSettings =
    HamletSettings doctype False xhtmlCloseStyle
  where
    doctype =
      "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" " ++
      "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"

debugHamletSettings :: HamletSettings
debugHamletSettings = HamletSettings "<!DOCTYPE html>" True htmlCloseStyle

htmlCloseStyle :: String -> CloseStyle
htmlCloseStyle s =
    if Set.member s htmlEmptyTags
        then NoClose
        else CloseSeparate

xhtmlCloseStyle :: String -> CloseStyle
xhtmlCloseStyle s =
    if Set.member s htmlEmptyTags
        then CloseInside
        else CloseSeparate

data CloseStyle = NoClose | CloseInside | CloseSeparate

parseConds :: HamletSettings
           -> ([(Deref, [Doc])] -> [(Deref, [Doc])])
           -> [Nest]
           -> Result ([(Deref, [Doc])], Maybe [Doc], [Nest])
parseConds set front (Nest LineElse inside:rest) = do
    inside' <- nestToDoc set inside
    Ok (front [], Just inside', rest)
parseConds set front (Nest (LineElseIf d) inside:rest) = do
    inside' <- nestToDoc set inside
    parseConds set (front . (:) (d, inside')) rest
parseConds _ front rest = Ok (front [], Nothing, rest)

doctypeNames :: [(String, String)]
doctypeNames =
    [ ("5", "<!DOCTYPE html>")
    , ("html", "<!DOCTYPE html>")
    , ("1.1", "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")
    , ("strict", "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
    ]

data Binding = BindVar Ident | BindConstr Ident [Ident] | BindTuple [Ident]
    deriving (Eq, Show, Read, Data, Typeable)
