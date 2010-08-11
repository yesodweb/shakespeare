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
import Data.List
import Data.Ord
import Data.Function
import Control.Arrow
import Data.Either

unsafeRenderTemplate :: FilePath -> HamletData url
                     -> (url -> [(String, String)] -> String) -> Html
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
    hd <- fmap concat $ mapM (getHD $ VarE render) docs
    hd' <- combineHDs hd
    let h = urt `AppE` LitE (StringL fp) `AppE` hd' `AppE` VarE render
    return $ LamE [VarP render] h

derefToExp :: [String] -> Exp
derefToExp = foldr1 AppE . map (varName []) . reverse

combineHDs :: [([String], Exp)] -> Q Exp
combineHDs [([], e)] = return e
combineHDs [([""], e)] = return e
combineHDs pairs = do
    pairs' <- mapM (\(x, y) -> do
                case y of
                    [([""], e)] -> return $ TupE [LitE $ StringL "", e]
                    _ -> do
                        let y' = map (\(a, b) ->
                                    (if null a then [""] else a, b)) y
                        y'' <- combineHDs y'
                        return $ TupE [LitE $ StringL x, y''])
            $ map (fst . head &&& map snd)
            $ groupBy ((==) `on` fst)
            $ sortBy (comparing fst)
            $ map (\(x, y) ->
                        case x of
                            [] -> ("", ([], y))
                            (x':xs) -> (x', (xs, y)))
            $ map head
            $ groupBy ((==) `on` fst)
            $ sortBy (comparing fst) pairs
    hm <- [|HDMap|]
    return $ hm `AppE` ListE pairs'


getHD :: Exp -> SimpleDoc -> Q [([String], Exp)]
getHD _ SDRaw{} = return []
getHD _ (SDVar x) = do
    th <- [|HDHtml . toHtml|]
    return [(x, th `AppE` derefToExp x)]
getHD _ (SDUrl hasParams x) = do
    th <- if hasParams then [|uncurry HDUrlParams|] else [|HDUrl|]
    return [(x, th `AppE` derefToExp x)]
getHD render (SDTemplate x) = do
    th <- [|HDHtml . toHtml|]
    return [(x, th `AppE` (derefToExp x `AppE` render))]
getHD render (SDCond xs edocs) = do
    hd <- fmap concat $ mapM (getHD render) $ edocs ++ concatMap snd xs
    bools <- mapM (go . fst) xs
    return $ hd ++ bools
  where
    go x = do
        tb <- [|HDBool|]
        return (x, tb `AppE` derefToExp x)
getHD render (SDMaybe x y docs ndocs) = do
    hd <- fmap concat $ mapM (getHD render) docs
    let (tops1, subs) = partitionEithers $ map go hd
    tops2 <- fmap concat $ mapM (getHD render) ndocs
    jsubs <- combineHDs subs
    let jsubs' = LamE [VarP $ mkName y] jsubs
    e <- [|\a -> HDMaybe . fmap a|]
    return $ (x, e `AppE` jsubs' `AppE` derefToExp x) : tops1 ++ tops2
  where
    go (a@(y':rest), e)
        | y == y' = Right (rest, e)
        | otherwise = Left (a, e)
    go ([], _) = error "getHD of SDMaybe"
getHD render (SDForall x y docs) = do
    hd <- fmap concat $ mapM (getHD render) docs
    let (tops, subs) = partitionEithers $ map go hd
    jsubs <- combineHDs subs
    let jsubs' = LamE [VarP $ mkName y] jsubs
    e <- [|\a -> HDList . map a|]
    return $ (x, e `AppE` jsubs' `AppE` derefToExp x) : tops
  where
    go (a@(y':rest), e)
        | y == y' = Right (rest, e)
        | otherwise = Left (a, e)
    go ([], _) = error "getHD of SDForall"
