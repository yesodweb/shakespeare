import Text.Hamlet

main :: IO ()
main = interact (toFunc "myTemp" . compactContent . foldr ($) [] . map htmlToContent . map nestedToHtml . nest . map parseLine . lines)
