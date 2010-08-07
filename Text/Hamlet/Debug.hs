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
import Data.List
import Data.Ord
import Data.Function
import Control.Arrow

unsafeRenderTemplate :: FilePath -> HamletData url
                     -> (url -> String) -> Html ()
unsafeRenderTemplate fp hd render = unsafePerformIO $ do
    contents <- fmap BSU.toString $ qRunIO $ S8.readFile fp
    temp <- parseHamletRT defaultHamletSettings contents
    renderHamletRT' True temp hd render

hamletFileDebug :: FilePath -> Q Exp
hamletFileDebug fp = do
    contents <- fmap BSU.toString $ qRunIO $ S8.readFile fp
    HamletRT docs <- qRunIO $ parseHamletRT defaultHamletSettings contents
    urt <- [|unsafeRenderTemplate|]
    render <- newName "render"
    hd <- fmap catMaybes $ mapM (getHD $ VarE render) docs
    hdm <- [|HDMap|]
    hd' <- combineHDs hd
    let h = urt `AppE` LitE (StringL fp) `AppE` hd' `AppE` VarE render
    return $ LamE [VarP render] h

derefToExp :: [String] -> Exp
derefToExp = foldr1 AppE . map (varName [])

combineHDs :: [([String], Exp)] -> Q Exp
combineHDs [([], y)] = return y
combineHDs pairs = do
    pairs' <- mapM (\(x, y) -> do
                y' <- combineHDs y
                return $ TupE [LitE $ StringL x, y'])
            $ map (fst . head &&& map snd)
            $ groupBy ((==) `on` fst)
            $ sortBy (comparing fst)
            $ map (\(x:xs, y) -> (x, (xs, y))) pairs
    hm <- [|HDMap|]
    return $ hm `AppE` ListE pairs'


getHD :: Exp -> SimpleDoc -> Q (Maybe ([String], Exp))
getHD _ SDRaw{} = return Nothing
getHD _ (SDVar x) = do
    th <- [|HDHtml . toHtml|]
    return $ Just (x, th `AppE` derefToExp x)
getHD _ (SDUrl hasParams x) = do
    th <- if hasParams then [|uncurry HDUrlParams|] else [|HDUrl|]
    return $ Just (x, th `AppE` derefToExp x)
getHD render (SDTemplate x) = do
    th <- [|HDHtml . toHtml|]
    return $ Just (x, th `AppE` (derefToExp x `AppE` render))
