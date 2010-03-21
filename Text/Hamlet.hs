module Text.Hamlet
    where

import Data.Char
import Data.Maybe

data Line = Line
    { lineTagName :: Maybe String
    , lineId :: Maybe String
    , lineClass :: Maybe String
    , lineContent :: Maybe String
    , lineSrc :: Maybe String
    }
    deriving Show

data Nested = Nested Line [Nested]
    deriving Show

data Piece = PTag String
           | PId String
           | PClass String
           | PContent String
           | PSrc String

parseDelim :: Char -> Maybe (String -> Piece)
parseDelim '%' = Just PTag
parseDelim '#' = Just PId
parseDelim '.' = Just PClass
parseDelim '@' = Just PSrc
parseDelim _ = Nothing

isDelimOrSpace :: Char -> Bool
isDelimOrSpace ' ' = True
isDelimOrSpace '\t' = True
isDelimOrSpace c = isJust $ parseDelim c

parsePieces :: String -> [Piece]
parsePieces "" = []
parsePieces s@(d:rest) =
    case parseDelim d of
        Just d' ->
            let (val, rest') = span (not . isDelimOrSpace) rest
             in d' val : parsePieces rest'
        Nothing -> [PContent $ dropWhile isSpace s]

parseLine :: String -> (Int, Line)
parseLine s =
    let (indent', s') = span isSpace s
        indent'' = sum $ map countSpace indent'
     in (indent'', foldr addPiece (Line
            { lineTagName = Nothing
            , lineId = Nothing
            , lineClass = Nothing
            , lineContent = Nothing
            , lineSrc = Nothing
            }) $ parsePieces s')

addPiece :: Piece -> Line -> Line
addPiece (PTag t) l = l { lineTagName = Just t }
addPiece (PId i) l = l { lineId = Just i }
addPiece (PClass c) l = l { lineClass = Just $ addClass c $ lineClass l }
addPiece (PContent c) l = l { lineContent = Just c }
addPiece (PSrc s) l = l { lineSrc = Just s }

addClass :: String -> Maybe String -> String
addClass s = maybe s (++ (' ' : s))

countSpace :: Char -> Int
countSpace ' ' = 1
countSpace '\t' = 4
countSpace _ = 0

nest :: [(Int, Line)] -> [Nested]
nest [] = []
nest ((i, l):rest) =
    let (deeper, rest') = span (\(i', _) -> i' > i) rest
     in Nested l (nest deeper) : nest rest'

data Html = Tag String [(String, Content)] Html
          | Html Content
          | HtmlList [Html]
    deriving Show

nestedToHtml :: Nested -> Html
nestedToHtml n =
  case n of
    Nested (Line Nothing Nothing Nothing (Just lcon) _) [] ->
        HtmlList $ map Html $ parseContent lcon
    Nested (Line ltn lid lcl lcon lsrc) nested ->
        let ltn' = fromMaybe "div" ltn
            lid' = maybe id (\x -> (:) ("id", Val x)) lid
            lcl' = maybe id (\x -> (:) ("class", Val x)) lcl
            lcon' = maybe id ((:) . HtmlList . map Html . parseContent) lcon
            lsrc' = case lsrc of
                        Nothing -> id
                        Just s ->
                            let key =
                                    case ltn' of
                                        "link" -> "href"
                                        "a" -> "href"
                                        _ -> "src"
                             in (:) (key, Url s)
         in Tag ltn' (lid' $ lcl' $ lsrc' [])
                $ HtmlList $ lcon'
                $ map nestedToHtml nested

parseContent :: String -> [Content]
parseContent [] = []
parseContent s =
    let (a, s') = break (== '$') s
        (b, c) = break (== '$') $ drop 1 s'
     in case (s', b, c) of
            ('$':_, (_:_), ('$':c')) -> Val a : Func b : parseContent c'
            _ -> [Val s]

data Content = Val String | Func String | Url String
    deriving Show

htmlToContent :: Html -> [Content] -> [Content]
htmlToContent (Html x) rest = x : rest
htmlToContent (HtmlList l) rest = foldr htmlToContent rest l
htmlToContent (Tag name attribs content) rest =
    Val ('<' : name) : attribsToContent attribs (
        Val ">" : htmlToContent content (
            if closeTag name
                then (Val ('<' : '/' : name ++ ">") : rest)
                else rest
        )
    )

compactContent :: [Content] -> [Content]
compactContent (Val x:Val y:rest) = compactContent $ Val (x ++ y) : rest
compactContent (x:y) = x : compactContent y
compactContent [] = []

attribsToContent :: [(String, Content)] -> [Content] -> [Content]
attribsToContent [] x = x
attribsToContent ((k, v):rest) x = Val (' ' : k ++ "=\"")
                                 : v
                                 : Val "\""
                                 : attribsToContent rest x

{-
printHtml :: Html -> String -> String
printHtml (Tag name attribs content) rest =
    '<' : name ++ printAttribs attribs (
        '>' : printHtml content
            (if closeTag name
                then ('<' : '/' : name ++ '>' : rest)
                else rest)
    )
printHtml (Html (Val s)) s' = s ++ s'
printHtml (Html (Func s)) s' = "FIXME" ++ s ++ s'
printHtml (HtmlList l) s = foldr printHtml s l
-}

closeTag :: String -> Bool
closeTag "img" = False
closeTag "link" = False
closeTag "meta" = False
closeTag "br" = False
closeTag "hr" = False
closeTag _ = True

{-
printAttribs :: [(String, Content)] -> String -> String
printAttribs [] s = s
printAttribs ((k, Val v):rest) s =
    ' ' : encodeHtml k ++ '=' : '"' : encodeHtml v ++ '"' : printAttribs rest s
printAttribs ((k, Func v):rest) s =
    ' ' : encodeHtml k ++ '=' : '"' : encodeHtml ("FIXME" ++ v) ++ '"' : printAttribs rest s
-}

toFunc :: String -> [Content] -> String
toFunc fn content = unlines $ (fn ++ " renderUrl obj = do") : map go content where
    go (Val s) = "    putStr " ++ show s
    go (Func s) = "    " ++ s ++ " obj >>= putStr"
    go (Url s) = "    putStr $ renderUrl $ " ++ s ++ " obj"
