{-# OPTIONS_HADDOCK hide #-}
-- | This module is only being exposed to work around a GHC bug, its API is not stable

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
module Text.Internal.Css where

import Data.List (intersperse, intercalate)
import Data.Text.Lazy.Builder (Builder, singleton, toLazyText, fromLazyText, fromString)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Data.Monoid (Monoid, mconcat, mappend, mempty)
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH.Syntax
import System.IO.Unsafe (unsafePerformIO)
import Text.ParserCombinators.Parsec (Parser, parse)
import Text.Shakespeare.Base hiding (Scope)
import Language.Haskell.TH
import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***), second)
import Text.IndentToBrace (i2b)
import Data.Functor.Identity (runIdentity)
import Text.Shakespeare (VarType (..))

type CssUrl url = (url -> [(T.Text, T.Text)] -> T.Text) -> Css

type DList a = [a] -> [a]

data Resolved = Resolved | Unresolved

-- Should mixins keep order (new version) or not (deprecated version)
data Order = Ordered | Unordered deriving (Lift)

type HasLeadingSpace = Bool

type family Str (a :: Resolved)
type instance Str 'Resolved = Builder
type instance Str 'Unresolved = Contents

data Block (a :: Resolved) where
  BlockResolved :: 
    { brSelectors :: !Builder
    , brAttrs     :: ![Attr 'Resolved]
    } -> Block 'Resolved
  BlockUnresolved ::
    { buSelectors      :: ![Contents]
    , buAttrsAndMixins :: ![Either (Attr 'Unresolved) Deref]
    , buBlocks         :: ![(HasLeadingSpace, Block 'Unresolved)]
    } -> Block 'Unresolved

data Mixin = Mixin
    { mixinAttrs :: ![Attr 'Resolved]
    , mixinBlocks :: ![(HasLeadingSpace, Block 'Resolved)]
    }
    deriving Lift
instance Semigroup Mixin where
    Mixin a x <> Mixin b y = Mixin (a ++ b) (x ++ y)
instance Monoid Mixin where
    mempty = Mixin mempty mempty

data TopLevel (a :: Resolved) where
    TopBlock   :: !(Block a) -> TopLevel a
    TopAtBlock :: !String -- name e.g., media
               -> !(Str a) -- selector
               -> ![TopLevel a]
               -> TopLevel a
    TopAtDecl  :: !String -> !(Str a) -> TopLevel a
    TopVar     :: !String -> !String -> TopLevel 'Unresolved

data Attr (a :: Resolved) where
  AttrResolved ::
    { attrResKey :: !Builder
    , attrResVal :: !Builder
    } -> Attr 'Resolved
  AttrUnresolved ::
    { attrUnresKey :: !Contents
    , attrUnresVal :: !Contents
    } -> Attr 'Unresolved

data Css = CssWhitespace ![TopLevel 'Resolved]
         | CssNoWhitespace ![TopLevel 'Resolved]

data Content = ContentRaw String
             | ContentVar Deref
             | ContentUrl Deref
             | ContentUrlParam Deref
             | ContentMixin Deref
    deriving (Show, Eq, Lift)

type Contents = [Content]

data CDData url = CDPlain Builder
                | CDUrl url
                | CDUrlParam (url, [(Text, Text)])
                | CDMixin Mixin

pack :: String -> Text
pack = T.pack

fromText :: Text -> Builder
fromText = TLB.fromText
{-# NOINLINE fromText #-}

class ToCss a where
    toCss :: a -> Builder

instance ToCss [Char] where toCss = fromLazyText . TL.pack
instance ToCss Text where toCss = fromText
instance ToCss TL.Text where toCss = fromLazyText

-- | Determine which identifiers are used by the given template, useful for
-- creating systems like yesod devel.
cssUsedIdentifiers :: Bool -- ^ perform the indent-to-brace conversion
                   -> Parser [TopLevel 'Unresolved]
                   -> String
                   -> [(Deref, VarType)]
cssUsedIdentifiers toi2b parseBlocks s' =
    concat $ either error id $ mapM (getVars scope0) contents
  where
    s = if toi2b then i2b s' else s'
    a = either (error . show) id $ parse parseBlocks s s
    (scope0, contents) = go a

    go :: [TopLevel 'Unresolved] -> (Scope, [Content])
    go [] = ([], [])
    go (TopAtDecl dec cs:rest) =
        (scope, rest'')
      where
        (scope, rest') = go rest
        rest'' =
            ContentRaw ('@' : dec ++ " ")
          : cs
         ++ ContentRaw ";"
          : rest'
    go (TopAtBlock _ _ nested:rest) =
        (scope1 ++ scope2, rest1 ++ rest2)
      where
        (scope1, rest1) = go nested
        (scope2, rest2) = go rest
    go (TopBlock (BlockUnresolved x y z):rest) =
        (scope1 ++ scope2, rest0 ++ rest1 ++ rest2)
      where
        rest0 = intercalate [ContentRaw ","] x ++ concatMap go' y
        (scope1, rest1) = go (map (TopBlock . snd) z)
        (scope2, rest2) = go rest
    go (TopVar k v:rest) =
        ((k, v):scope, rest')
      where
        (scope, rest') = go rest
    go' :: Either (Attr 'Unresolved) Deref -> [Content]
    go' (Left (AttrUnresolved k v)) = k ++ (v :: [Content])
    go' (Right m) = [ContentMixin m]

cssFileDebug :: Bool  -- ^ perform the indent-to-brace conversion
             -> Q Exp
             -> Parser [TopLevel 'Unresolved]
             -> FilePath
             -> Q Exp
cssFileDebug toi2b parseBlocks' parseBlocks fp = do
    s <- readFileQ fp
    let vs = cssUsedIdentifiers toi2b parseBlocks s
    c <- mapM vtToExp vs
    cr <- [|cssRuntime toi2b|]
    parseBlocks'' <- parseBlocks'
    return $ cr `AppE` parseBlocks'' `AppE` (LitE $ StringL fp) `AppE` ListE c

runtimePrependSelector :: Builder -> (HasLeadingSpace, Block 'Resolved) -> Block 'Resolved
runtimePrependSelector builder (hsl, BlockResolved x b) =
    BlockResolved (builder <> addSpace x) b
  where
    addSpace = if hsl then (TLB.singleton ' ' <>) else id

combineSelectors :: HasLeadingSpace
                 -> [Contents]
                 -> [Contents]
                 -> [Contents]
combineSelectors hsl a b = do
    a' <- a
    b' <- b
    return $ a' ++ addSpace b'
  where
    addSpace
        | hsl = (ContentRaw " " :)
        | otherwise = id

blockRuntime :: [(Deref, CDData url)]
             -> (url -> [(Text, Text)] -> Text)
             -> Block 'Unresolved
             -> Either String (DList (Block 'Resolved))
-- FIXME share code with blockToCss
blockRuntime cd render' (BlockUnresolved x attrsAndMixins z) = do
    x' <- mapM go' $ intercalate [ContentRaw ","] x
    attrs' <- mapM (either resolveAttr getMixinAttrs) attrsAndMixins
    blocks' <- mapM (either (const $ Right []) getMixinBlocks) attrsAndMixins
    z' <- mapM (subGo x) z -- FIXME use difflists again
    Right $ \rest -> BlockResolved
        { brSelectors = mconcat x'
        , brAttrs     = concat attrs'
        }
        : map (runtimePrependSelector $ mconcat x') (concat blocks')
        ++ foldr ($) rest z'
    {-
    (:) (Css' (mconcat $ map go' $ intercalate [ContentRaw "," ] x) (map go'' y))
    . foldr (.) id (map (subGo x) z)
    -}
  where
    go' = contentToBuilderRT cd render'

    getMixin :: Deref -> Either String Mixin
    getMixin d =
        case lookup d cd of
            Nothing -> Left $ "Mixin not found: " ++ show d
            Just (CDMixin m) -> Right m
            Just _ -> Left $ "For " ++ show d ++ ", expected Mixin"

    getMixinAttrs :: Deref -> Either String [Attr 'Resolved]
    getMixinAttrs = fmap mixinAttrs . getMixin

    getMixinBlocks :: Deref -> Either String [(HasLeadingSpace, Block 'Resolved)]
    getMixinBlocks = fmap mixinBlocks . getMixin

    resolveAttr :: Attr 'Unresolved -> Either String [Attr 'Resolved]
    resolveAttr (AttrUnresolved k v) =
      let eAttr = AttrResolved <$> (mconcat <$> mapM go' k) <*> (mconcat <$> mapM go' v)
       in fmap (:[]) eAttr

    subGo :: [Contents] -- ^ parent selectors
          -> (HasLeadingSpace, Block 'Unresolved)
          -> Either String (DList (Block 'Resolved))
    subGo x' (hls, BlockUnresolved a b c) =
        let a' = combineSelectors hls x' a
         in blockRuntime cd render' (BlockUnresolved a' b c)

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
contentToBuilderRT _ _ ContentMixin{} = Left "contentToBuilderRT ContentMixin"

cssRuntime :: Bool -- ^ i2b?
           -> Parser [TopLevel 'Unresolved]
           -> FilePath
           -> [(Deref, CDData url)]
           -> (url -> [(Text, Text)] -> Text)
           -> Css
cssRuntime toi2b parseBlocks fp cd render' = unsafePerformIO $ do
    s' <- readUtf8FileString fp
    let s = if toi2b then i2b s' else s'
    let a = either (error . show) id $ parse parseBlocks s s
    return $ CssWhitespace $ goTop [] a
  where
    goTop :: [(String, String)] -- ^ scope
          -> [TopLevel 'Unresolved]
          -> [TopLevel 'Resolved]
    goTop _ [] = []
    goTop scope (TopAtDecl dec cs':rest) =
        TopAtDecl dec cs : goTop scope rest
      where
        cs = either error mconcat $ mapM (contentToBuilderRT cd render') cs'
    goTop scope (TopBlock b:rest) =
        map TopBlock (either error ($ []) $ blockRuntime (addScope scope) render' b) ++
        goTop scope rest
    goTop scope (TopAtBlock name s' nested:rest) =
        TopAtBlock name s (goTop scope nested) :
        goTop scope rest
      where
        s = either error mconcat $ mapM (contentToBuilderRT cd render') s'
    goTop scope (TopVar k v:rest) = goTop ((k, v):scope) rest

    addScope scope = map (DerefIdent . Ident *** CDPlain . fromString) scope ++ cd

vtToExp :: (Deref, VarType) -> Q Exp
vtToExp (d, vt) = do
    d' <- lift d
    c' <- c vt
    return $ TupE
#if MIN_VERSION_template_haskell(2,16,0)
      $ map Just
#endif
      [d', c' `AppE` derefToExp [] d]
  where
    c :: VarType -> Q Exp
    c VTPlain = [|CDPlain . toCss|]
    c VTUrl = [|CDUrl|]
    c VTUrlParam = [|CDUrlParam|]
    c VTMixin = [|CDMixin|]

getVars :: [(String, String)] -> Content -> Either String [(Deref, VarType)]
getVars _ ContentRaw{} = return []
getVars scope (ContentVar d) =
    case lookupD d scope of
        Just _ -> return []
        Nothing -> return [(d, VTPlain)]
getVars scope (ContentUrl d) =
    case lookupD d scope of
        Nothing -> return [(d, VTUrl)]
        Just s -> Left $ "Expected URL for " ++ s
getVars scope (ContentUrlParam d) =
    case lookupD d scope of
        Nothing -> return [(d, VTUrlParam)]
        Just s -> Left $ "Expected URLParam for " ++ s
getVars scope (ContentMixin d) =
    case lookupD d scope of
        Nothing -> return [(d, VTMixin)]
        Just s -> Left $ "Expected Mixin for " ++ s

lookupD :: Deref -> [(String, b)] -> Maybe String
lookupD (DerefIdent (Ident s)) scope =
    case lookup s scope of
        Nothing -> Nothing
        Just _ -> Just s
lookupD _ _ = Nothing

compressTopLevel :: TopLevel 'Unresolved
                 -> TopLevel 'Unresolved
compressTopLevel (TopBlock b) = TopBlock $ compressBlock b
compressTopLevel (TopAtBlock name s b) = TopAtBlock name s $ map compressTopLevel b
compressTopLevel x@TopAtDecl{} = x
compressTopLevel x@TopVar{} = x

compressBlock :: Block 'Unresolved
              -> Block 'Unresolved
compressBlock (BlockUnresolved x y blocks) =
    BlockUnresolved (map cc x) (map go y) (map (second compressBlock) blocks)
  where
    go :: Either (Attr 'Unresolved) Deref -> Either (Attr 'Unresolved) Deref
    go (Left (AttrUnresolved k v)) = Left $ AttrUnresolved (cc k) (cc v)
    go (Right m) = Right m
    cc :: Contents -> Contents
    cc [] = []
    cc (ContentRaw a:ContentRaw b:c) = cc $ ContentRaw (a ++ b) : c
    cc (a:b) = a : cc b

blockToMixin :: Name
             -> Scope
             -> Block 'Unresolved
             -> Q Exp
blockToMixin r scope (BlockUnresolved _sel props subblocks) =
    -- TODO: preserve the CPS in @mixinBlocks@ below
    [| let attrsAndMixins = $(processAttrsAndDerefs r scope props)
        in Mixin
            { mixinAttrs =
                concatMap (either (:[]) mixinAttrs) attrsAndMixins
            , mixinBlocks =
                concat $
                  $(listE $ map subGo subblocks)
                  ++ map (either (const []) mixinBlocks) attrsAndMixins
            }
    |]
  where
    -- We don't use the @hls@ to combine selectors, because the parent
    -- selector for a mixin is the dummy @mixin@ selector. But we may want
    -- to know later if the block needs a leading space, because the mixin
    -- might include an @&@ which needs to mix correctly with the parent
    -- block's selector.
    subGo (hls, BlockUnresolved sel' b c) =
      [| map (\x -> ($(lift hls), x))
           $ $(blockToCss r scope $ BlockUnresolved sel' b c) []
      |]
        
blockToCss :: Name
           -> Scope
           -> Block 'Unresolved
           -> ExpQ
blockToCss r scope (BlockUnresolved sel props subblocks) =
    [| let attrsAndMixins = $(processAttrsAndDerefs r scope props)
           selToBuilder = $(selectorToBuilder r scope sel)
       in ( BlockResolved
            { brSelectors = selToBuilder
            , brAttrs     = concatMap (either (:[]) mixinAttrs) attrsAndMixins
            }:)
          . foldr (.) id $(listE $ map subGo subblocks)
          . (fmap
                (runtimePrependSelector selToBuilder)
                (concatMap (either (const []) mixinBlocks) attrsAndMixins) ++)
    |]
  where
    subGo :: (HasLeadingSpace, Block 'Unresolved) -> Q Exp
    subGo (hls, BlockUnresolved sel' b c) =
        let sel'' = combineSelectors hls sel sel'
         in blockToCss r scope $ BlockUnresolved sel'' b c


processAttrsAndDerefs ::
     Name
  -> Scope
  -> [Either (Attr 'Unresolved) Deref]
  -> Q Exp -- ^ Either (Attr 'Resolved) Mixin
processAttrsAndDerefs r scope props = listE $ map go props
  where
    go (Right deref) = pure $ ConE 'Right `AppE` (derefToExp [] deref)
    go (Left (AttrUnresolved x y)) =
          conE 'Left `appE`
            ( conE 'AttrResolved
                `appE` (contentsToBuilder r scope x)
                `appE` (contentsToBuilder r scope y)
            )

selectorToBuilder :: Name -> Scope -> [Contents] -> Q Exp
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
contentToBuilder _ _ ContentMixin{} = error "contentToBuilder on ContentMixin"

type Scope = [(String, String)]

topLevelsToCassius :: [TopLevel 'Unresolved]
                   -> Q Exp
topLevelsToCassius a = do
    r <- newName "_render"
    lamE [varP r] $ appE [|CssNoWhitespace . foldr ($) []|] $ fmap ListE $ go r [] a
  where
    go _ _ [] = return []
    go r scope (TopBlock b:rest) = do
        e <- [|(++) $ map TopBlock ($(blockToCss r scope b) [])|]
        es <- go r scope rest
        return $ e : es
    go r scope (TopAtBlock name s nested:rest) = do
        let s' = contentsToBuilder r scope s
        inner <- go r scope nested
        e <- [|(:) $ TopAtBlock $(lift name) $(s') (foldr ($) [] $(return $ ListE inner))|]
        es <- go r scope rest
        return $ e : es
    go r scope (TopAtDecl dec cs:rest) = do
        e <- [|(:) $ TopAtDecl $(lift dec) $(contentsToBuilder r scope cs)|]
        es <- go r scope rest
        return $ e : es
    go r scope (TopVar k v:rest) = go r ((k, v) : scope) rest

blocksToCassius :: Name
                -> Scope
                -> [Block 'Unresolved]
                -> Q Exp
blocksToCassius r scope a = do
    appE [|foldr ($) []|] $ listE $ map (blockToCss r scope) a

renderCss :: Css -> TL.Text
renderCss css =
    toLazyText $ mconcat $ map go tops
  where
    (haveWhiteSpace, tops) =
        case css of
            CssWhitespace x -> (True, x)
            CssNoWhitespace x -> (False, x)
    go = goWith mempty

    goWith indent (TopBlock x) = renderBlock haveWhiteSpace indent x
    goWith indent (TopAtBlock name s x) =
        indent <>
        fromText (pack $ concat ["@", name, " "]) <> s <> startBlock <>
        foldr mappend (endBlockWith indent) (map (goWith innerIndent) x)
      where
        innerIndent
            | haveWhiteSpace = indent <> fromString "    "
            | otherwise = mempty
    goWith indent (TopAtDecl dec cs) =
        indent <>
        fromText (pack $ concat ["@", dec, " "]) <> cs <> endDecl

    startBlock
        | haveWhiteSpace = fromString " {\n"
        | otherwise = singleton '{'

    endBlockWith indent
        | haveWhiteSpace = indent <> fromString "}\n"
        | otherwise = singleton '}'

    endDecl
        | haveWhiteSpace = fromString ";\n"
        | otherwise = singleton ';'

renderBlock :: Bool -- ^ have whitespace?
            -> Builder -- ^ indentation
            -> Block 'Resolved
            -> Builder
renderBlock haveWhiteSpace indent (BlockResolved sel attrs)
    | null attrs = mempty
    | otherwise = startSelect
               <> sel
               <> startBlock
               <> mconcat (intersperse endDecl $ map renderAttr attrs)
               <> endBlock
  where
    renderAttr (AttrResolved k v) = startDecl <> k <> colon <> v

    colon
        | haveWhiteSpace = fromString ": "
        | otherwise = singleton ':'

    startSelect
        | haveWhiteSpace = indent
        | otherwise = mempty

    startBlock
        | haveWhiteSpace = fromString " {\n"
        | otherwise = singleton '{'

    endBlock
        | haveWhiteSpace = fromString ";\n" `mappend` indent `mappend` fromString "}\n"
        | otherwise = singleton '}'

    startDecl
        | haveWhiteSpace = indent `mappend` fromString "    "
        | otherwise = mempty

    endDecl
        | haveWhiteSpace = fromString ";\n"
        | otherwise = singleton ';'

instance Lift (Attr a) where
  lift = \case
    AttrResolved k v -> [|AttrResolved $(liftBuilder k) $(liftBuilder v)|]
    AttrUnresolved k v -> [|AttrUnresolved k v|]
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = unsafeCodeCoerce . lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = unsafeTExpCoerce . lift
#endif

#if MIN_VERSION_template_haskell(2,17,0)
liftBuilder :: Quote m => Builder -> m Exp
#else
liftBuilder :: Builder -> Q Exp
#endif
liftBuilder b = [|fromText $ pack $(lift $ TL.unpack $ toLazyText b)|]

instance Lift (Block a) where
  lift = \case
    BlockResolved a b -> [|BlockResolved $(liftBuilder a) b|]
    BlockUnresolved a b c -> [|BlockUnresolved a b c|]
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = unsafeCodeCoerce . lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = unsafeTExpCoerce . lift
#endif
