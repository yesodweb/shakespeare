module Old.Hamlet
    ( parse'
    , render'
    ) where

import Control.Applicative ((<$>), Applicative (..))
import Control.Monad
import Control.Arrow
import Data.Data
import Data.List (intercalate)
import Text.ParserCombinators.Parsec hiding (Line)
import Text.Shakespeare (Deref (..), Ident (..))
import Text.Hamlet.Parse (Line (..), Content (..))
import Old.Utf8 (renderDeref)

renderAttr (mderef, name, val) = concat
    [ " "
    , case mderef of
        Nothing -> ""
        Just deref -> concat
            [ ":"
            , renderDeref deref
            , ":"
            ]
    , name
    , "=\""
    , concatMap renderContent val
    , "\""
    ]

renderClass c = ' ': '.' : concatMap renderContent c

renderContent (ContentRaw s) = s
renderContent (ContentVar d) = concat ["#{", renderDeref d, "}"]
renderContent (ContentUrl False d) = concat ["@{", renderDeref d, "}"]
renderContent (ContentUrl True d) = concat ["@?{", renderDeref d, "}"]
renderContent (ContentEmbed d) = concat ["^{", renderDeref d, "}"]

renderLine' (indent, x) = concat [replicate indent ' ', renderLine x, "\n"]

renderLine (LineForall deref (Ident i)) = concat
    [ "$forall "
    , i
    , " <- "
    , renderDeref deref
    ]
renderLine (LineIf deref) = concat
    [ "$if "
    , renderDeref deref
    ]
renderLine (LineElseIf deref) = concat
    [ "$elseif "
    , renderDeref deref
    ]
renderLine LineElse = "$else"
renderLine (LineMaybe deref (Ident i)) = concat
    [ "$maybe "
    , i
    , " <- "
    , renderDeref deref
    ]
renderLine LineNothing = "$nothing"
renderLine (LineTag tn attrs content classes) = concat
    [ "<"
    , tn
    , concatMap renderAttr attrs
    , concatMap renderClass classes
    , ">"
    , concatMap renderContent content
    ]
renderLine (LineContent c) = '\\' : concatMap renderContent c

data Result v = Error String | Ok v
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
         comment <|>
         backslash <|>
         controlIf <|>
         controlElseIf <|>
         (try (string "$else") >> many (oneOf " \t") >> eol >> return LineElse) <|>
         controlMaybe <|>
         (try (string "$nothing") >> many (oneOf " \t") >> eol >> return LineNothing) <|>
         controlForall <|>
         tag <|>
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
    doctype = do
        try $ string "!!!" >> eol
        return $ LineContent [ContentRaw $ hamletDoctype set ++ "\n"]
    comment = do
        _ <- try $ string "$#"
        _ <- many $ noneOf "\r\n"
        eol
        return $ LineContent []
    backslash = do
        _ <- char '\\'
        (eol >> return (LineContent [ContentRaw "\n"]))
            <|> (LineContent <$> content InContent)
    controlIf = do
        _ <- try $ string "$if"
        spaces
        x <- deref False
        _ <- many $ oneOf " \t"
        eol
        return $ LineIf x
    controlElseIf = do
        _ <- try $ string "$elseif"
        spaces
        x <- deref False
        _ <- many $ oneOf " \t"
        eol
        return $ LineElseIf x
    controlMaybe = do
        _ <- try $ string "$maybe"
        spaces
        x <- deref False
        spaces
        y <- ident
        _ <- many $ oneOf " \t"
        eol
        return $ LineMaybe x y
    controlForall = do
        _ <- try $ string "$forall"
        spaces
        x <- deref False
        spaces
        y <- ident
        _ <- many $ oneOf " \t"
        eol
        return $ LineForall x y
    tag = do
        x <- tagName <|> tagIdent <|> tagClass <|> tagAttrib
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
    derefSingle = derefParens <|> fmap DerefIdent ident
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

-- | Settings for parsing of a hamlet document.
data HamletSettings = HamletSettings
    {
      -- | The value to replace a \"!!!\" with. Do not include the trailing
      -- newline.
      hamletDoctype :: String
      -- | 'True' means to close empty tags (eg, img) with a trailing slash, ie
      -- XML-style empty tags. 'False' uses HTML-style.
    , hamletCloseEmpties :: Bool
      -- | Should we put a newline after closing a tag?
    , hamletCloseNewline :: Bool
    }

-- | Defaults settings: HTML5 doctype and HTML-style empty tags.
defaultHamletSettings :: HamletSettings
defaultHamletSettings = HamletSettings "<!DOCTYPE html>" False False

xhtmlHamletSettings :: HamletSettings
xhtmlHamletSettings =
    HamletSettings doctype True False
  where
    doctype =
      "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" " ++
      "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"

debugHamletSettings :: HamletSettings
debugHamletSettings = HamletSettings "<!DOCTYPE html>" False True

data CloseStyle = NoClose | CloseInside | CloseSeparate

-- FIXME A breaking change, but move closeTag to be a record in the
-- HamletSettings datatype. Would allow more precise XML encodings.
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

parse' set s =
    case parseLines defaultHamletSettings s of
        Error e -> error e
        Ok x -> x

render' = concatMap renderLine'
