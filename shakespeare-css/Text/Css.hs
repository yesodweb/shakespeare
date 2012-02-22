{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE CPP #-}
module Text.Css where

import Data.List (intersperse, intercalate)
import Data.Text.Lazy.Builder (Builder, fromText, singleton, toLazyText, fromLazyText, fromString)
import qualified Data.Text.Lazy as TL
import Data.Monoid (mconcat, mappend, mempty)
import Data.Text (Text, pack)
import Language.Haskell.TH.Syntax
import System.IO.Unsafe (unsafePerformIO)
import Text.ParserCombinators.Parsec (Parser, parse)
import Text.Shakespeare.Base hiding (Scope)
import Language.Haskell.TH
import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))

class ToCss a where
    toCss :: a -> Builder

instance ToCss [Char] where toCss = fromLazyText . TL.pack
instance ToCss Text where toCss = fromText
instance ToCss TL.Text where toCss = fromLazyText

data Css' = Css'
    { _cssSelectors :: Builder
    , _cssAttributes :: [(Builder, Builder)]
    }
data CssTop = AtBlock String Builder [Css'] | Css Css' | AtDecl String String

type Css = [CssTop]

data Content = ContentRaw String
             | ContentVar Deref
             | ContentUrl Deref
             | ContentUrlParam Deref
    deriving (Show, Eq)

type Contents = [Content]
type ContentPair = (Contents, Contents)

data VarType = VTPlain | VTUrl | VTUrlParam
    deriving Show

data CDData url = CDPlain Builder
                | CDUrl url
                | CDUrlParam (url, [(Text, Text)])

cssFileDebug :: Q Exp -> Parser [TopLevel] -> FilePath -> Q Exp
cssFileDebug parseBlocks' parseBlocks fp = do
    s <- fmap TL.unpack $ qRunIO $ readUtf8File fp
#ifdef GHC_7_4
    qAddDependentFile fp
#endif
    let a = either (error . show) id $ parse parseBlocks s s
    let (scope, contents) = go a
    vs <- mapM (getVars scope) contents
    c <- mapM vtToExp $ concat vs
    cr <- [|cssRuntime|]
    parseBlocks'' <- parseBlocks'
    return $ cr `AppE` parseBlocks'' `AppE` (LitE $ StringL fp) `AppE` ListE c
  where
    go :: [TopLevel] -> ([(String, String)], [Content])
    go [] = ([], [])
    go (TopAtDecl dec cs:rest) =
        (scope, rest'')
      where
        (scope, rest') = go rest
        rest'' = ContentRaw (concat
            [ "@"
            , dec
            , cs
            , ";"
            ]) : rest'
    go (TopAtBlock _ _ blocks:rest) =
        (scope1 ++ scope2, rest1 ++ rest2)
      where
        (scope1, rest1) = go (map TopBlock blocks)
        (scope2, rest2) = go rest
    go (TopBlock (Block x y z):rest) =
        (scope1 ++ scope2, rest0 ++ rest1 ++ rest2)
      where
        rest0 = intercalate [ContentRaw ","] x ++ concatMap go' y
        (scope1, rest1) = go (map TopBlock z)
        (scope2, rest2) = go rest
    go (TopVar k v:rest) =
        ((k, v):scope, rest')
      where
        (scope, rest') = go rest
    go' (k, v) = k ++ v

combineSelectors :: Selector -> Selector -> Selector
combineSelectors a b = do
    a' <- a
    b' <- b
    return $ a' ++ ContentRaw " " : b'

blockRuntime :: [(Deref, CDData url)]
             -> (url -> [(Text, Text)] -> Text)
             -> Block
             -> Either String ([Css'] -> [Css'])
-- FIXME share code with blockToCss
blockRuntime cd render' (Block x y z) = do
    x' <- mapM go' $ intercalate [ContentRaw ","] x
    y' <- mapM go'' y
    z' <- mapM (subGo x) z -- FIXME use difflists again
    Right $ \rest -> Css' (mconcat x') y' : foldr ($) rest z'
    {-
    (:) (Css' (mconcat $ map go' $ intercalate [ContentRaw "," ] x) (map go'' y))
    . foldr (.) id (map (subGo x) z)
    -}
  where
    go' = contentToBuilderRT cd render'

    go'' :: ([Content], [Content]) -> Either String (Builder, Builder)
    go'' (k, v) = (,) <$> (mconcat <$> mapM go' k) <*> (mconcat <$> mapM go' v)

    subGo :: Selector -> Block -> Either String ([Css'] -> [Css'])
    subGo x' (Block a b c) =
        blockRuntime cd render' (Block a' b c)
      where
        a' = combineSelectors x' a

contentToBuilderRT :: [(Deref, CDData url)]
                   -> (url -> [(Text, Text)] -> Text)
                   -> Content
                   -> Either String Builder
contentToBuilderRT _ _ (ContentRaw s) = Right $ fromText $ pack s
contentToBuilderRT cd _ (ContentVar d) =
    case lookup d cd of
        Just (CDPlain s) -> Right s
        _ -> Left $ show d ++ ": expected CDPlain"
contentToBuilderRT cd render' (ContentUrl d) =
    case lookup d cd of
        Just (CDUrl u) -> Right $ fromText $ render' u []
        _ -> Left $ show d ++ ": expected CDUrl"
contentToBuilderRT cd render' (ContentUrlParam d) =
    case lookup d cd of
        Just (CDUrlParam (u, p)) ->
            Right $ fromText $ render' u p
        _ -> Left $ show d ++ ": expected CDUrlParam"

cssRuntime :: Parser [TopLevel]
           -> FilePath
           -> [(Deref, CDData url)]
           -> (url -> [(Text, Text)] -> Text)
           -> Css
cssRuntime parseBlocks fp cd render' = unsafePerformIO $ do
    s <- fmap TL.unpack $ qRunIO $ readUtf8File fp
    let a = either (error . show) id $ parse parseBlocks s s
    return $ goTop [] a
  where
    goTop :: [(String, String)] -> [TopLevel] -> Css
    goTop _ [] = []
    goTop scope (TopAtDecl dec cs:rest) = AtDecl dec cs : goTop scope rest
    goTop scope (TopBlock b:rest) =
        map Css (either error ($[]) $ blockRuntime (addScope scope) render' b) ++
        goTop scope rest
    goTop scope (TopAtBlock name s' b:rest) =
        AtBlock name s (foldr (either error id . blockRuntime (addScope scope) render') [] b) :
        goTop scope rest
      where
        s = either error mconcat $ mapM (contentToBuilderRT cd render') s'
    goTop scope (TopVar k v:rest) = goTop ((k, v):scope) rest

    addScope scope = map (DerefIdent . Ident *** CDPlain . fromString) scope ++ cd

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

getVars :: Monad m => [(String, String)] -> Content -> m [(Deref, VarType)]
getVars _ ContentRaw{} = return []
getVars scope (ContentVar d) =
    case lookupD d scope of
        Just _ -> return []
        Nothing -> return [(d, VTPlain)]
getVars scope (ContentUrl d) =
    case lookupD d scope of
        Nothing -> return [(d, VTUrl)]
        Just s -> fail $ "Expected URL for " ++ s
getVars scope (ContentUrlParam d) =
    case lookupD d scope of
        Nothing -> return [(d, VTUrlParam)]
        Just s -> fail $ "Expected URLParam for " ++ s

lookupD :: Deref -> [(String, b)] -> Maybe String
lookupD (DerefIdent (Ident s)) scope =
    case lookup s scope of
        Nothing -> Nothing
        Just _ -> Just s
lookupD _ _ = Nothing

data Block = Block Selector Pairs [Block]
    deriving Show

data TopLevel = TopBlock Block
              | TopAtBlock
                    { _atBlockName :: String
                    , _atBlockSelector :: Contents
                    , _atBlockInner :: [Block]
                    }
              | TopAtDecl String String
              | TopVar String String

type Pairs = [Pair]

type Pair = (Contents, Contents)

type Selector = [Contents]

compressTopLevel :: TopLevel -> TopLevel
compressTopLevel (TopBlock b) = TopBlock $ compressBlock b
compressTopLevel (TopAtBlock name s b) = TopAtBlock name s $ map compressBlock b
compressTopLevel x@TopAtDecl{} = x
compressTopLevel x@TopVar{} = x

compressBlock :: Block -> Block
compressBlock (Block x y blocks) =
    Block (map cc x) (map go y) (map compressBlock blocks)
  where
    go (k, v) = (cc k, cc v)
    cc [] = []
    cc (ContentRaw a:ContentRaw b:c) = cc $ ContentRaw (a ++ b) : c
    cc (a:b) = a : cc b

blockToCss :: Name -> Scope -> Block -> Q Exp
blockToCss r scope (Block sel props subblocks) =
    [|(:) (Css' $(selectorToBuilder r scope sel) $(listE $ map go props))
      . foldr (.) id $(listE $ map subGo subblocks)
    |]
  where
    go (x, y) = tupE [contentsToBuilder r scope x, contentsToBuilder r scope y]
    subGo (Block sel' b c) =
        blockToCss r scope $ Block sel'' b c
      where
        sel'' = combineSelectors sel sel'

selectorToBuilder :: Name -> Scope -> Selector -> Q Exp
selectorToBuilder r scope sels =
    contentsToBuilder r scope $ intercalate [ContentRaw ","] sels

contentsToBuilder :: Name -> Scope -> [Content] -> Q Exp
contentsToBuilder r scope contents =
    appE [|mconcat|] $ listE $ map (contentToBuilder r scope) contents

contentToBuilder :: Name -> Scope -> Content -> Q Exp
contentToBuilder _ _ (ContentRaw x) =
    [|fromText . pack|] `appE` litE (StringL x)
contentToBuilder _ scope (ContentVar d) =
    case d of
        DerefIdent (Ident s)
            | Just val <- lookup s scope -> [|fromText . pack|] `appE` litE (StringL val)
        _ -> [|toCss|] `appE` return (derefToExp [] d)
contentToBuilder r _ (ContentUrl u) =
    [|fromText|] `appE`
        (varE r `appE` return (derefToExp [] u) `appE` listE [])
contentToBuilder r _ (ContentUrlParam u) =
    [|fromText|] `appE`
        ([|uncurry|] `appE` varE r `appE` return (derefToExp [] u))

type Scope = [(String, String)]

topLevelsToCassius :: [TopLevel] -> Q Exp
topLevelsToCassius a = do
    r <- newName "_render"
    lamE [varP r] $ appE [|foldr ($) []|] $ fmap ListE $ go r [] a
  where
    go _ _ [] = return []
    go r scope (TopBlock b:rest) = do
        e <- [|(++) $ map Css ($(blockToCss r scope b) [])|]
        es <- go r scope rest
        return $ e : es
    go r scope (TopAtBlock name s b:rest) = do
        let s' = contentsToBuilder r scope s
        e <- [|(:) $ AtBlock $(lift name) $(s') $(blocksToCassius r scope b)|]
        es <- go r scope rest
        return $ e : es
    go r scope (TopAtDecl dec cs:rest) = do
        e <- [|(:) $ AtDecl $(lift dec) $(lift cs)|]
        es <- go r scope rest
        return $ e : es
    go r scope (TopVar k v:rest) = go r ((k, v) : scope) rest

blocksToCassius :: Name -> Scope -> [Block] -> Q Exp
blocksToCassius r scope a = do
    appE [|foldr ($) []|] $ listE $ map (blockToCss r scope) a

renderCss :: Css -> TL.Text
renderCss =
    toLazyText . mconcat . map go -- FIXME use a foldr
  where
    go (Css x) = renderCss' x
    go (AtBlock name s x) =
        fromText (pack $ concat ["@", name, " "]) `mappend`
        s `mappend`
        singleton '{' `mappend`
        foldr mappend (singleton '}') (map renderCss' x)
    go (AtDecl dec cs) = fromText (pack $ concat ["@", dec, " "]) `mappend`
                      fromText (pack cs) `mappend`
                      singleton ';'

renderCss' :: Css' -> Builder
renderCss' (Css' _x []) = mempty
renderCss' (Css' x y) =
    x
    `mappend` singleton '{'
    `mappend` mconcat (intersperse (singleton ';') $ map go' y)
    `mappend` singleton '}'
  where
    go' (k, v) = k `mappend` singleton ':' `mappend` v
