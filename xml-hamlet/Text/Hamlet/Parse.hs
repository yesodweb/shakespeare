{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
module Text.Hamlet.Parse
    ( Result (..)
    , Content (..)
    , Doc (..)
    , parseDoc
    )
    where

import Text.Shakespeare.Base
import Control.Applicative ((<$>), Applicative (..))
import Control.Monad
import Data.Data
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

data Content = ContentRaw String
             | ContentVar Deref
             | ContentEmbed Deref
    deriving (Show, Eq, Read, Data, Typeable)

data Line = LineForall Deref Ident
          | LineIf Deref
          | LineElseIf Deref
          | LineElse
          | LineWith [(Deref, Ident)]
          | LineMaybe Deref Ident
          | LineNothing
          | LineTag
            { _lineTagName :: String
            , _lineAttr :: [(Maybe Deref, String, [Content])]
            , _lineContent :: [Content]
            }
          | LineContent [Content]
    deriving (Eq, Show, Read)

parseLines :: String -> Result [(Int, Line)]
parseLines s =
    case parse (many parseLine) s s of
        Left e -> Error $ show e
        Right x -> Ok x

parseLine :: Parser (Int, Line)
parseLine = do
    ss <- fmap sum $ many ((char ' ' >> return 1) <|>
                           (char '\t' >> return 4))
    x <- comment <|>
         htmlComment <|>
         backslash <|>
         controlIf <|>
         controlElseIf <|>
         (try (string "$else") >> spaceTabs >> eol >> return LineElse) <|>
         controlMaybe <|>
         (try (string "$nothing") >> spaceTabs >> eol >> return LineNothing) <|>
         controlForall <|>
         controlWith <|>
         angle <|>
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
        y <- ident
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
    content' cr = contentHash <|> contentCaret <|> contentReg cr
    contentHash = do
        x <- parseHash
        case x of
            Left str -> return $ ContentRaw str
            Right deref -> return $ ContentVar deref
    contentCaret = do
        x <- parseCaret
        case x of
            Left str -> return $ ContentRaw str
            Right deref -> return $ ContentEmbed deref
    contentReg InContent = (ContentRaw . return) <$> noneOf "#@^\r\n"
    contentReg NotInQuotes = (ContentRaw . return) <$> noneOf "@^#. \t\n\r>"
    contentReg NotInQuotesAttr = (ContentRaw . return) <$> noneOf "@^ \t\n\r>"
    contentReg InQuotes = (ContentRaw . return) <$> noneOf "#@^\\\"\n\r>"
    tagAttribValue notInQuotes = do
        cr <- (char '"' >> return InQuotes) <|> return notInQuotes
        content cr
    tagCond = do
        _ <- char ':'
        d <- parseDeref
        _ <- char ':'
        tagAttrib (Just d)
    tagAttrib cond = do
        s <- many1 $ noneOf " \t=\r\n>"
        v <- (do
            _ <- char '='
            s' <- tagAttribValue NotInQuotesAttr
            return s') <|> return []
        return $ TagAttrib (cond, s, v)
    tag' = foldr tag'' ("div", [])
    tag'' (TagName s) (_, y) = (s, y)
    tag'' (TagAttrib s) (x, y) = (x, s : y)
    ident = Ident <$> many1 (alphaNum <|> char '_' <|> char '\'')
    angle = do
        _ <- char '<'
        name' <- many  $ noneOf " \t\r\n>"
        let name = if null name' then "div" else name'
        xs <- many $ try ((many $ oneOf " \t") >>
              (tagCond <|> tagAttrib Nothing))
        _ <- many $ oneOf " \t"
        c <- (eol >> return []) <|> (do
            _ <- char '>'
            c <- content InContent
            return c)
        let (tn, attr) = tag' $ TagName name : xs
        return $ LineTag tn attr c

data TagPiece = TagName String
              | TagAttrib (Maybe Deref, String, [Content])
    deriving Show

data ContentRule = InQuotes | NotInQuotes | NotInQuotesAttr | InContent

data Nest = Nest Line [Nest]

nestLines :: [(Int, Line)] -> [Nest]
nestLines [] = []
nestLines ((i, l):rest) =
    let (deeper, rest') = span (\(i', _) -> i' > i) rest
     in Nest l (nestLines deeper) : nestLines rest'

data Doc = DocForall Deref Ident [Doc]
         | DocWith [(Deref,Ident)] [Doc]
         | DocCond [(Deref, [Doc])] (Maybe [Doc])
         | DocMaybe Deref Ident [Doc] (Maybe [Doc])
         | DocTag String [(Maybe Deref, String, [Content])] [Doc]
         | DocContent Content
         -- FIXME PIs
    deriving (Show, Eq, Read, Data, Typeable)

nestToDoc :: [Nest] -> Result [Doc]
nestToDoc [] = Ok []
nestToDoc (Nest (LineForall d i) inside:rest) = do
    inside' <- nestToDoc inside
    rest' <- nestToDoc rest
    Ok $ DocForall d i inside' : rest'
nestToDoc (Nest (LineWith dis) inside:rest) = do
    inside' <- nestToDoc inside
    rest' <- nestToDoc rest
    Ok $ DocWith dis inside' : rest'
nestToDoc (Nest (LineIf d) inside:rest) = do
    inside' <- nestToDoc inside
    (ifs, el, rest') <- parseConds ((:) (d, inside')) rest
    rest'' <- nestToDoc rest'
    Ok $ DocCond ifs el : rest''
nestToDoc (Nest (LineMaybe d i) inside:rest) = do
    inside' <- nestToDoc inside
    (nothing, rest') <-
        case rest of
            Nest LineNothing ninside:x -> do
                ninside' <- nestToDoc ninside
                return (Just ninside', x)
            _ -> return (Nothing, rest)
    rest'' <- nestToDoc rest'
    Ok $ DocMaybe d i inside' nothing : rest''
nestToDoc (Nest (LineTag tn attrs content) inside:rest) = do
    inside' <- nestToDoc inside
    rest' <- nestToDoc rest
    Ok $ (DocTag tn attrs $ map DocContent content ++ inside') : rest'
nestToDoc (Nest (LineContent content) inside:rest) = do
    inside' <- nestToDoc inside
    rest' <- nestToDoc rest
    Ok $ map DocContent content ++ inside' ++ rest'
nestToDoc (Nest (LineElseIf _) _:_) = Error "Unexpected elseif"
nestToDoc (Nest LineElse _:_) = Error "Unexpected else"
nestToDoc (Nest LineNothing _:_) = Error "Unexpected nothing"

parseDoc :: String -> Result [Doc]
parseDoc s = do
    ls <- parseLines s
    let notEmpty (_, LineContent []) = False
        notEmpty _ = True
    let ns = nestLines $ filter notEmpty ls
    ds <- nestToDoc ns
    return ds

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
