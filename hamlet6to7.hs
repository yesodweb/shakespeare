import qualified Old.Julius as J
import qualified Old.Cassius as C
import qualified Old.Hamlet as H
import System.Environment (getArgs)

import Text.ParserCombinators.Parsec
import qualified Text.Julius as JN
import qualified Text.Cassius as CN
import qualified Text.Hamlet.Parse as HN

import Data.Char (toUpper)

main = getArgs >>= mapM_ go

go fp = do
    putStrLn $ "Checking " ++ fp
    case reverse $ takeWhile (/= '.') $ reverse fp of
        "julius" -> readFile fp >>= jelper >>= writeFile fp7
        "cassius" -> readFile fp >>= celper >>= writeFile fp7
        "hamlet" -> readFile fp >>= helper HN.defaultHamletSettings >>= writeFile fp7
        "hs" -> readFile fp >>= hsHelper fp7
        _ -> return ()
  where
    fp7 = fp ++ ".7"
    write = writeFile fp7
    check checker = do
        x <- checker `fmap` readFile fp7
        if x then return () else putStrLn $ "### Error parsing: " ++ fp7

hsHelper fp s = do
    let contents = either (error . show) id $ parse parseHs s s
    contents' <- concat `fmap` mapM renderContent contents
    writeFile fp contents'

renderContent (ContentRaw s) = return s
renderContent (ContentHamlet False s) = helper HN.defaultHamletSettings s
renderContent (ContentHamlet True s) = helper HN.xhtmlHamletSettings s
renderContent (ContentCassius s) = celper s
renderContent (ContentJulius s) = jelper s

data Content = ContentRaw String
             | ContentHamlet Bool String
             | ContentCassius String
             | ContentJulius String
parseHs =
    concat `fmap` many1 parseContent
  where
    parseContent = parseHamlet
                   <|> parseXhamlet
                   <|> parseCassius
                   <|> parseJulius
                   <|> (return . ContentRaw . return) `fmap` anyChar
    parseHamlet = do
        start <- startP "hamlet"
        inside <- manyTill anyChar $ try $ string "|]"
        return [ContentRaw start, ContentHamlet False inside, ContentRaw "|]"]
    parseXhamlet = do
        start <- try $ string "[$xhamlet|" <|> string "[xhamlet|"
        inside <- manyTill anyChar $ try $ string "|]"
        return [ContentRaw start, ContentHamlet True inside, ContentRaw "|]"]
    parseCassius = do
        start <- try $ string "[$cassius|" <|> string "[cassius|"
        inside <- manyTill anyChar $ try $ string "|]"
        return [ContentRaw start, ContentCassius inside, ContentRaw "|]"]
    parseJulius = do
        start <- try $ string "[$julius|" <|> string "[julius|"
        inside <- manyTill anyChar $ try $ string "|]"
        return [ContentRaw start, ContentJulius inside, ContentRaw "|]"]
    startP n = try (string $ concat ["[$", n, "|"])
               <|> try (string $ concat ["[", n, "|"])
               <|> try (string $ concat ["[", map toUpper n, "|"])
               <|> try (do
                            a <- string "\n#if "
                            b <- many1 $ noneOf "\r\n"
                            c <- string "\r\n" <|> string "\n"
                            d <- many $ char ' '
                            e <- string $ concat ["[", n, "|"]
                            f <- string "\r\n" <|> string "\n"
                            g <- string "#else"
                            h <- string "\r\n" <|> string "\n"
                            i <- many $ char ' '
                            j <- string $ concat ["[$", n, "|"]
                            k <- string "\r\n" <|> string "\n"
                            l <- string "#endif"
                            m <- string "\r\n" <|> string "\n"
                            return $ concat [a, b, c, d, e, f, g, h, i, j, k, l, m]
               )

jelper s = do
    let x = J.parse s
    let y = J.render x
    case parse JN.parseContents y y of
        Right z
            | JN.compressContents x == JN.compressContents z -> return ()
            | otherwise -> putStrLn "### Mismatch in Julius"
        _ -> putStrLn "Could not parse Julius"
    return y

celper s = do
    let x = C.parse s
    let y = C.render' x
    case parse CN.parseBlocks y y of
        Right z
            | x == z -> return ()
            | otherwise -> putStrLn "### Mismatch in Cassius"
        _ -> putStrLn "Could not parse Cassius"
    return y

helper set s = do
    let x = H.parse' set s
    let y = H.render' x
    case HN.parseLines set y of
        HN.Ok z
            | x == z -> return ()
            | otherwise -> putStrLn "### Mismatch in Hamlet"
        _ -> putStrLn "Could not parse Hamlet"
    return y
