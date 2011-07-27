data Url = Home | Img
renderUrl' Home = "http://localhost/"
renderUrl' Img = "http://localhost/image.png"

data Obj = Obj
    { foo :: Url
    , bar :: IO String
    }

main = myTemp renderUrl' $ Obj Img (return "some bar value")

myTemp renderUrl obj = do
    putStr "<html><head><title>Foo Bar Baz</title></head><body><h1>Hello World</h1><div id=\"content\"><div class=\"foo\">Bar Baz</div>Plain Content<img src=\""
    putStr $ renderUrl $ foo obj
    putStr "\">"
    bar obj >>= putStr
    putStr "</div></body></html>"
