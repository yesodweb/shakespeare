module Text.Hamlet.Tag
    where

data Content = Val String | Func String | Url String

data Tag = Tag String [(String, [Content])] Tag
         | Content Content
         | TagList [Tag]

closeTag :: String -> Bool
closeTag "img" = False
closeTag "link" = False
closeTag "meta" = False
closeTag "br" = False
closeTag "hr" = False
closeTag _ = True

compressContent :: [Content] -> [Content]
compressContent (Val "":rest) = compressContent rest
compressContent (Val x:Val y:rest) = compressContent $ Val (x ++ y) : rest
compressContent (x:rest) = x : compressContent rest
compressContent [] = []

compressTag :: Tag -> [Content]
compressTag (Tag n attrs t) =
    let inner = compressTag t
        end = if closeTag n || not (null inner)
                then [Val $ "</" ++ n ++ ">"]
                else []
        start = Val $ "<" ++ n
        attrs' = concatMap attrToContent attrs
     in compressContent $ start : attrs' ++ [Val ">"] ++ inner ++ end

attrToContent :: (String, [Content]) -> [Content]
attrToContent (k, []) = [Val $ ' ' : k]
attrToContent (k, v) = (Val $ ' ' : k ++ "=\"") : v ++ Val "\""
