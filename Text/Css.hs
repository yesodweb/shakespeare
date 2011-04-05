{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
module Text.Css where

import Data.Text.Lazy.Builder (Builder, fromText)
import qualified Data.Text.Lazy as TL
import Data.Monoid (mconcat)
import Data.Text (Text, pack)
import Language.Haskell.TH.Syntax
import System.IO.Unsafe (unsafePerformIO)
import Text.Hamlet.Quasi (readUtf8File)
import Text.ParserCombinators.Parsec (parse)
import Text.Shakespeare

class ToCss a where
    toCss :: a -> Builder

data Css' = Css'
    { _cssSelectors :: Builder
    , _cssAttributes :: [(Builder, Builder)]
    }

data Content = ContentRaw String
             | ContentVar Deref
             | ContentUrl Deref
             | ContentUrlParam Deref
    deriving (Show, Eq)

type Contents = [Content]
type ContentPair = (Contents, Contents)

data VarType = VTPlain | VTUrl | VTUrlParam

data CDData url = CDPlain Builder
                | CDUrl url
                | CDUrlParam (url, [(Text, Text)])

--cssFileDebug :: FilePath -> Q Exp
cssFileDebug parseBlocks' parseBlocks fp = do
    s <- fmap TL.unpack $ qRunIO $ readUtf8File fp
    let a = either (error . show) id $ parse parseBlocks s s
    c <- mapM vtToExp $ concatMap getVars $ concatMap go a
    cr <- [|cssRuntime|]
    parseBlocks'' <- parseBlocks'
    return $ cr `AppE` parseBlocks'' `AppE` (LitE $ StringL fp) `AppE` ListE c
  where
    go (x, y) = x ++ concatMap go' y
    go' (k, v) = k ++ v

--cssRuntime :: FilePath -> [(Deref, CDData url)] -> Cassius url
cssRuntime parseBlocks fp cd render' = unsafePerformIO $ do
    s <- fmap TL.unpack $ qRunIO $ readUtf8File fp
    let a = either (error . show) id $ parse parseBlocks s s
    return $ map go a
  where
    go :: (Contents, [ContentPair]) -> Css'
    go (x, y) = Css' (mconcat $ map go' x) $ map go'' y
    go' :: Content -> Builder
    go' (ContentRaw s) = fromText $ pack s
    go' (ContentVar d) =
        case lookup d cd of
            Just (CDPlain s) -> s
            _ -> error $ show d ++ ": expected CDPlain"
    go' (ContentUrl d) =
        case lookup d cd of
            Just (CDUrl u) -> fromText $ render' u []
            _ -> error $ show d ++ ": expected CDUrl"
    go' (ContentUrlParam d) =
        case lookup d cd of
            Just (CDUrlParam (u, p)) ->
                fromText $ render' u p
            _ -> error $ show d ++ ": expected CDUrlParam"
    go'' (k, v) = (mconcat $ map go' k, mconcat $ map go' v)

vtToExp :: (Deref, VarType) -> Q Exp
vtToExp (d, vt) = do
    d' <- lift d
    c' <- c vt
    return $ TupE [d', c' `AppE` derefToExp [] d]
  where
    c :: VarType -> Q Exp
    c VTPlain = [|CDPlain . toCss|]
    c VTUrl = [|CDUrl|]
    c VTUrlParam = [|CDUrlParam|]

getVars :: Content -> [(Deref, VarType)]
getVars ContentRaw{} = []
getVars (ContentVar d) = [(d, VTPlain)]
getVars (ContentUrl d) = [(d, VTUrl)]
getVars (ContentUrlParam d) = [(d, VTUrlParam)]
