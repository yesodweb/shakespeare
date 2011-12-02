{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
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
import Text.Shakespeare.Base hiding (Scope)
import Language.Haskell.TH
import Control.Applicative ((<$>), (<*>))

class ToCss a where
    toCss :: a -> Builder

instance ToCss [Char] where toCss = fromLazyText . TL.pack
instance ToCss Text where toCss = fromText
instance ToCss TL.Text where toCss = fromLazyText

data Css' = Css'
    { _cssSelectors :: Builder
    , _cssAttributes :: [(Builder, Builder)]
    }
data CssTop = Media String [Css'] | Css Css' | Charset String

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
    go (TopCharset cs) = [ContentRaw "@charset ", ContentRaw cs, ContentRaw ";"]
    go (MediaBlock _ blocks) = concatMap (go . TopBlock) blocks
    go (TopBlock (Block x y z)) =
        intercalate [ContentRaw ","] x ++
        concatMap go' y ++
        concatMap (go . TopBlock) z
    go TopVar{} = error "FIXME cssFileDebug does not support TopVar"
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
    go' :: Content -> Either String Builder
    go' (ContentRaw s) = Right $ fromText $ pack s
    go' (ContentVar d) =
        case lookup d cd of
            Just (CDPlain s) -> Right s
            _ -> Left $ show d ++ ": expected CDPlain"
    go' (ContentUrl d) =
        case lookup d cd of
            Just (CDUrl u) -> Right $ fromText $ render' u []
            _ -> Left $ show d ++ ": expected CDUrl"
    go' (ContentUrlParam d) =
        case lookup d cd of
            Just (CDUrlParam (u, p)) ->
                Right $ fromText $ render' u p
            _ -> Left $ show d ++ ": expected CDUrlParam"

    go'' :: ([Content], [Content]) -> Either String (Builder, Builder)
    go'' (k, v) = (,) <$> (mconcat <$> mapM go' k) <*> (mconcat <$> mapM go' v)

    subGo :: Selector -> Block -> Either String ([Css'] -> [Css'])
    subGo x' (Block a b c) =
        blockRuntime cd render' (Block a' b c)
      where
        a' = combineSelectors x' a

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
    goTop (TopCharset cs) x = Charset cs : x
    goTop (TopBlock b) x = map Css (either error ($[]) $ blockRuntime cd render' b) ++ x
    goTop TopVar{} _ = error "FIXME cssRuntime does not support TopVar"
    goTop (MediaBlock s b) x = Media s (foldr (either error id . blockRuntime cd render') [] b) : x

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

data TopLevel = TopBlock Block | MediaBlock String [Block] | TopCharset String | TopVar String String

type Pairs = [Pair]

type Pair = (Contents, Contents)

type Selector = [Contents]

compressTopLevel :: TopLevel -> TopLevel
compressTopLevel (TopBlock b) = TopBlock $ compressBlock b
compressTopLevel (MediaBlock s b) = MediaBlock s $ map compressBlock b
compressTopLevel x@TopCharset{} = x
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
    go r scope (MediaBlock s b:rest) = do
        e <- [|(:) $ Media $(lift s) $(blocksToCassius r scope b)|]
        es <- go r scope rest
        return $ e : es
    go r scope (TopCharset cs:rest) = do
        e <- [|(:) $ Charset $(lift cs)|]
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
    go (Media s x) =
        fromText (pack "@media ") `mappend`
        fromText (pack s) `mappend`
        singleton '{' `mappend`
        foldr mappend (singleton '}') (map renderCss' x)
    go (Charset cs) = fromText (pack "@charset ") `mappend`
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
