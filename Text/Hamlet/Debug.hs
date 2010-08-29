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
import Control.Arrow
import Data.Either
import Control.Monad (forM)

unsafeRenderTemplate :: FilePath -> HamletMap url
                     -> (url -> [(String, String)] -> String) -> Html
unsafeRenderTemplate fp hd render = unsafePerformIO $ do
    contents <- fmap BSU.toString $ S8.readFile fp
    temp <- parseHamletRT defaultHamletSettings contents
    renderHamletRT' True temp hd render

hamletFileDebug :: FilePath -> Q Exp
hamletFileDebug fp = do
    contents <- fmap BSU.toString $ qRunIO $ S8.readFile fp
    HamletRT docs <- qRunIO $ parseHamletRT defaultHamletSettings contents
    urt <- [|unsafeRenderTemplate|]
    render <- newName "render"
    let hd = combineDVals $ concatMap getHD docs
    hd' <- liftDVals (VarE render) hd
    let h = urt `AppE` LitE (StringL fp) `AppE` hd' `AppE` VarE render
    return $ LamE [VarP render] h

derefToExp :: [Exp] -> Exp
derefToExp = foldr1 AppE . reverse

type DVal = ([Exp], DVal')
data DVal' = DHtml
           | DUrl
           | DUrlParam
           | DTemplate
           | DBool
           | DMaybe [([String], DVal)]
           | DList [([String], DVal)]
    deriving (Show, Eq)

liftDVals :: Exp -> [([String], DVal)] -> Q Exp
liftDVals render pairs = do
    pairs' <- forM pairs $ \(k, d) -> do
        let k' = ListE $ map (LitE . StringL) k
        d' <- liftDVal render d
        return $ TupE [k', d']
    return $ ListE pairs'

liftDVal :: Exp -> DVal -> Q Exp
liftDVal _ (x, DHtml) = do
    f <- [|HDHtml . toHtml|]
    return $ f `AppE` derefToExp x
liftDVal _ (x, DUrl) = do
    f <- [|HDUrl|]
    return $ f `AppE` derefToExp x
liftDVal _ (x, DUrlParam) = do
    f <- [|uncurry HDUrlParams|]
    return $ f `AppE` derefToExp x
liftDVal render (x, DTemplate) = do
    f <- [|HDHtml|]
    return $ f `AppE` (derefToExp x `AppE` render)
liftDVal _ (x, DBool) = do
    f <- [|HDBool|]
    return $ f `AppE` derefToExp x
liftDVal render (x, DMaybe each) = do
    var <- newName "_var"
    each' <- liftDVals render $ map (second $ replaceFirst $ VarE var) each
    let each'' = LamE [VarP var] each'
    hdlist <- [|HDMaybe|]
    map' <- [|fmap|]
    return $ hdlist `AppE` (map' `AppE` each'' `AppE` derefToExp x)
liftDVal render (x, DList each) = do
    var <- newName "_var"
    each' <- liftDVals render $ map (second $ replaceFirst $ VarE var) each
    let each'' = LamE [VarP var] each'
    hdlist <- [|HDList|]
    map' <- [|map|]
    return $ hdlist `AppE` (map' `AppE` each'' `AppE` derefToExp x)

combineDVals :: [([String], DVal)] -> [([String], DVal)]
combineDVals [] = []
combineDVals ((x1, y1):rest) =
    case matches of
        [] -> (x1, y1) : combineDVals rest
        ys -> (x1, foldr combine' y1 ys) : combineDVals nomatch
  where
    matches = map snd $ filter (\(x, _) -> x == x1) rest
    nomatch = filter (\(x, _) -> x /= x1) rest
    combine' (a, x) (b, y)
        | a == b = (a, combine x y)
        | otherwise = error $ "Bad parameters to combine': " ++ show ((a, x), (b, y))
    combine (DList x) (DList y) = DList $ combineDVals $ x ++ y
    combine (DMaybe x) (DMaybe y) = DMaybe $ combineDVals $ x ++ y
    combine x y
        | x == y = x
    combine x y = error $ "Bad parameters to combine: " ++ show (x, y)

varNames :: [String] -> [Exp]
varNames = map $ varName []
getHD :: SimpleDoc -> [([String], DVal)]
getHD SDRaw{} = []
getHD (SDVar x) = [(x, (varNames x, DHtml))]
getHD (SDUrl hasParams x) =
    [(x, (varNames x, if hasParams then DUrlParam else DUrl))]
getHD (SDTemplate x) = [(x, (varNames x, DTemplate))]
getHD (SDCond xs edocs) =
    let hd = concatMap getHD $ edocs ++ concatMap snd xs
        bools = map (\(x, _) -> (x, (varNames x, DBool))) xs
     in hd ++ bools
getHD (SDMaybe x y docs ndocs) =
    (x, (varNames x, DMaybe subs)) : tops ++ ntops
  where
    hd = concatMap getHD docs
    (tops, subs) = partitionEithers $ map go hd
    ntops = concatMap getHD ndocs
    go (a@(y':rest), e)
        | y == y' = Right (rest, e)
        | otherwise = Left (a, e)
    go ([], _) = error "getHD of SDMaybe"
getHD (SDForall x y docs) =
     (x, (varNames x, DList subs)) : tops
  where
    hd = concatMap getHD docs
    (tops, subs) = partitionEithers $ map go hd
    go (a@(y':rest), e)
        | y == y' = Right (rest, e)
        | otherwise = Left (a, e)
    go ([], _) = error "getHD of SDForall"

replaceFirst :: Exp -> DVal -> DVal
replaceFirst x (_:y, z) = (x:y, z)
replaceFirst _ _ = error "replaceFirst on something empty"
