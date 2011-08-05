{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
module Text.Css where

import Data.List (intersperse, intercalate)
import Data.Text.Lazy.Builder (Builder, fromText, singleton, toLazyText, fromLazyText)
import qualified Data.Text.Lazy as TL
import Data.Monoid (mconcat, mappend, mempty)
import Data.Text (Text, pack)
import Language.Haskell.TH.Syntax
import System.IO.Unsafe (unsafePerformIO)
import Text.ParserCombinators.Parsec (Parser, parse)
import Text.Shakespeare
import Language.Haskell.TH

class ToCss a where
    toCss :: a -> Builder

instance ToCss [Char] where toCss = fromLazyText . TL.pack
instance ToCss Text where toCss = fromText
instance ToCss TL.Text where toCss = fromLazyText

data Css' = Css'
    { _cssSelectors :: Builder
    , _cssAttributes :: [(Builder, Builder)]
    }
data CssTop = Media String [Css'] | Css Css'

type Css = [CssTop]

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

cssFileDebug :: Q Exp -> Parser [TopLevel] -> FilePath -> Q Exp
cssFileDebug parseBlocks' parseBlocks fp = do
    s <- fmap TL.unpack $ qRunIO $ readUtf8File fp
    let a = either (error . show) id $ parse parseBlocks s s
    c <- mapM vtToExp $ concatMap getVars $ concatMap go a
    cr <- [|cssRuntime|]
    parseBlocks'' <- parseBlocks'
    return $ cr `AppE` parseBlocks'' `AppE` (LitE $ StringL fp) `AppE` ListE c
  where
    go :: TopLevel -> [Content] -- FIXME use blockToCss
    go (MediaBlock _ blocks) = concatMap (go . TopBlock) blocks
    go (TopBlock (Block x y z)) =
        intercalate [ContentRaw ","] x ++
        concatMap go' y ++
        concatMap (go . TopBlock) z
    go' (k, v) = k ++ v

combineSelectors :: Selector -> Selector -> Selector
combineSelectors a b = do
    a' <- a
    b' <- b
    return $ a' ++ ContentRaw " " : b'

cssRuntime :: Parser [TopLevel]
           -> FilePath
           -> [(Deref, CDData url)]
           -> (url -> [(Text, Text)] -> Text)
           -> Css
cssRuntime parseBlocks fp cd render' = unsafePerformIO $ do
    s <- fmap TL.unpack $ qRunIO $ readUtf8File fp
    let a = either (error . show) id $ parse parseBlocks s s
    return $ foldr ($) [] $ map goTop a
  where
    goTop :: TopLevel -> Css -> Css
    goTop (TopBlock b) x = map Css (go b []) ++ x
    goTop (MediaBlock s b) x = Media s (foldr go [] b) : x
    go :: Block -> [Css'] -> [Css']
    -- FIXME share code with blockToCss
    go (Block x y z) =
        (:) (Css' (mconcat $ map go' $ intercalate [ContentRaw "," ] x) (map go'' y))
        . foldr (.) id (map (subGo x) z)
    subGo :: Selector -> Block -> [Css'] -> [Css']
    subGo x (Block a b c) =
        go (Block a' b c)
      where
        a' = combineSelectors x a
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

data Block = Block Selector Pairs [Block]

data TopLevel = TopBlock Block | MediaBlock String [Block]

type Pairs = [Pair]

type Pair = (Contents, Contents)

type Selector = [Contents]

compressTopLevel :: TopLevel -> TopLevel
compressTopLevel (TopBlock b) = TopBlock $ compressBlock b
compressTopLevel (MediaBlock s b) = MediaBlock s $ map compressBlock b

compressBlock :: Block -> Block
compressBlock (Block x y blocks) =
    Block (map cc x) (map go y) (map compressBlock blocks)
  where
    go (k, v) = (cc k, cc v)
    cc [] = []
    cc (ContentRaw a:ContentRaw b:c) = cc $ ContentRaw (a ++ b) : c
    cc (a:b) = a : cc b

blockToCss :: Name -> Block -> Q Exp
blockToCss r (Block sel props subblocks) =
    [|(:) (Css' $(selectorToBuilder r sel) $(listE $ map go props))
      . foldr (.) id $(listE $ map subGo subblocks)
    |]
  where
    go (x, y) = tupE [contentsToBuilder r x, contentsToBuilder r y]
    subGo (Block sel' b c) =
        blockToCss r $ Block sel'' b c
      where
        sel'' = combineSelectors sel sel'

selectorToBuilder :: Name -> Selector -> Q Exp
selectorToBuilder r sels =
    contentsToBuilder r $ intercalate [ContentRaw ","] sels

contentsToBuilder :: Name -> [Content] -> Q Exp
contentsToBuilder r contents =
    appE [|mconcat|] $ listE $ map (contentToBuilder r) contents

contentToBuilder :: Name -> Content -> Q Exp
contentToBuilder _ (ContentRaw x) =
    [|fromText . pack|] `appE` litE (StringL x)
contentToBuilder _ (ContentVar d) =
    [|toCss|] `appE` return (derefToExp [] d)
contentToBuilder r (ContentUrl u) =
    [|fromText|] `appE`
        (varE r `appE` return (derefToExp [] u) `appE` listE [])
contentToBuilder r (ContentUrlParam u) =
    [|fromText|] `appE`
        ([|uncurry|] `appE` varE r `appE` return (derefToExp [] u))

topLevelsToCassius :: [TopLevel] -> Q Exp
topLevelsToCassius a = do
    r <- newName "_render"
    lamE [varP r] $ appE [|foldr ($) []|] $ listE $ map (go r) a
  where
    go r (TopBlock b) = [|(++) $ map Css ($(blockToCss r b) [])|]
    go r (MediaBlock s b) = [|(:) $ Media $(lift s) $(blocksToCassius r b)|]

blocksToCassius :: Name -> [Block] -> Q Exp
blocksToCassius r a = do
    appE [|foldr ($) []|] $ listE $ map (blockToCss r) a

renderCss :: Css -> TL.Text
renderCss =
    toLazyText . mconcat . map go -- FIXME use a foldr
  where
    go (Css x) = renderCss' x
    go (Media s x) =
        fromText (pack "@media ") `mappend`
        fromText (pack s) `mappend`
        singleton '{' `mappend`
        foldr mappend (singleton '}') (map renderCss' x)

renderCss' :: Css' -> Builder
renderCss' (Css' _x []) = mempty
renderCss' (Css' x y) =
    x
    `mappend` singleton '{'
    `mappend` mconcat (intersperse (singleton ';') $ map go' y)
    `mappend` singleton '}'
  where
    go' (k, v) = k `mappend` singleton ':' `mappend` v
