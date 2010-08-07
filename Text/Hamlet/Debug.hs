{-# LANGUAGE TemplateHaskell #-}
module Text.Hamlet.Debug
    ( hamletFileDebug
    ) where

import Text.Hamlet.Parse
import Text.Hamlet.Quasi
import Text.Hamlet.RT
import Language.Haskell.TH.Syntax
import qualified Data.ByteString.Char8 as S8
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.UTF8 as BSU
import Text.Blaze
import Data.Maybe (catMaybes)

unsafeRenderTemplate :: FilePath -> HamletData url
                     -> (url -> String) -> Html ()
unsafeRenderTemplate fp hd render = unsafePerformIO $ do
    contents <- fmap BSU.toString $ qRunIO $ S8.readFile fp
    temp <- parseHamletRT defaultHamletSettings contents
    renderHamletRT temp hd render

hamletFileDebug :: FilePath -> Q Exp
hamletFileDebug fp = do
    contents <- fmap BSU.toString $ qRunIO $ S8.readFile fp
    HamletRT docs <- qRunIO $ parseHamletRT defaultHamletSettings contents
    urt <- [|unsafeRenderTemplate|]
    hd <- fmap catMaybes $ mapM getHD docs
    hdm <- [|HDMap|]
    return $ urt `AppE` LitE (StringL fp) `AppE` (hdm `AppE` ListE hd)

getHD :: SimpleDoc -> Q (Maybe Exp)
getHD (SDVar [x]) = do
    th <- [|HDHtml . toHtml|]
    return $ Just $ TupE [LitE $ StringL x, th `AppE` VarE (mkName x)]
getHD _ = return Nothing
