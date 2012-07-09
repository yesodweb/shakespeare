{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)
import Network.Wai.Handler.Warp (run)
import System.Console.CmdArgs hiding (def)
import Text.Printf (printf)
import System.Directory (canonicalizePath)
import Control.Monad (unless)
import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Gzip
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as S8
import Control.Arrow ((***))
import Data.Text (Text, pack)
import qualified Data.Text as T
import Network.Wai
import Control.Monad.IO.Class (liftIO)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Text.Lucius (luciusRT)
import Text.Hamlet (defaultHamletSettings)
import Text.Hamlet.RT (parseHamletRT, renderHamletRT)
import Network.HTTP.Types (status200)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import qualified Data.Text.Lazy as TL
import Blaze.ByteString.Builder.Char.Utf8 (fromLazyText)
import WaiAppStatic.Mime (defaultMimeMap, mimeByExt, defaultMimeType)
import WaiAppStatic.Types (ssIndices, toPiece, ssGetMimeType, fileName)
import Data.String (fromString)
import Data.Maybe (mapMaybe)

data Args = Args
    { docroot :: FilePath
    , index :: [FilePath]
    , port :: Int
    , noindex :: Bool
    , quiet :: Bool
    , verbose :: Bool
    , mime :: [(String, String)]
    }
    deriving (Show, Data, Typeable)

defaultArgs :: Args
defaultArgs = Args "." ["index.html", "index.htm"] 3000 False False False []

main :: IO ()
main = do
    Args {..} <- cmdArgs defaultArgs
    let mime' = map (pack *** S8.pack) mime
    let mimeMap = Map.fromList mime' `Map.union` defaultMimeMap
    docroot' <- canonicalizePath docroot
    unless quiet $ printf "Serving directory %s on port %d with %s index files.\n" docroot' port (if noindex then "no" else show index)
    let middle = gzip def
               . (if verbose then logStdoutDev else id)
               . autohead
               . shake docroot
    run port $ middle $ staticApp (defaultFileServerSettings $ fromString docroot)
        { ssIndices = if noindex then [] else mapMaybe (toPiece . pack) index
        , ssGetMimeType = return . mimeByExt mimeMap defaultMimeType . fileName
        }

shake :: FilePath -> Middleware
shake docroot app req
    | any unsafe p = app req
    | null p = app req
    | ".hamlet" `T.isSuffixOf` l = liftIO $ hamlet pr
    | ".lucius" `T.isSuffixOf` l = liftIO $ lucius pr
    | otherwise = app req
  where
    p = pathInfo req
    pr = T.intercalate "/" $ T.pack docroot : p
    l = last p

unsafe :: Text -> Bool
unsafe s
    | T.null s = False
    | T.head s == '.' = True
    | otherwise = T.any (== '/') s

readFileUtf8 :: Text -> IO String
readFileUtf8 fp = do
    bs <- S8.readFile $ T.unpack fp
    let t = decodeUtf8With lenientDecode bs
    return $ T.unpack t

hamlet :: Text -> IO Response
hamlet fp = do
    str <- readFileUtf8 fp
    hrt <- parseHamletRT defaultHamletSettings str
    html <- renderHamletRT hrt [] (error "No URLs allowed")
    return $ ResponseBuilder status200 [("Content-Type", "text/html; charset=utf-8")] $ renderHtmlBuilder html

lucius :: Text -> IO Response
lucius fp = do
    str <- readFileUtf8 fp
    let text = either error id $ luciusRT (TL.pack str) []
    return $ ResponseBuilder status200 [("Content-Type", "text/css; charset=utf-8")] $ fromLazyText text
