{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
module Text.Css where

import Data.List (intersperse, intercalate)
import Data.Text.Lazy.Builder (Builder, singleton, toLazyText, fromLazyText, fromString)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Data.Monoid (Monoid, mconcat, mappend, mempty)
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

#if MIN_VERSION_base(4,5,0)
import Data.Monoid ((<>))
#else
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}
#endif

type CssUrl url = (url -> [(T.Text, T.Text)] -> T.Text) -> Css

type DList a = [a] -> [a]

-- FIXME great use case for data kinds
data Resolved
data Unresolved

type family Selector a
type instance Selector Resolved = Builder
type instance Selector Unresolved = [Contents]

type family ChildBlocks a
type instance ChildBlocks Resolved = ()
type instance ChildBlocks Unresolved = [(HasLeadingSpace, Block Unresolved)]

type HasLeadingSpace = Bool

type family Str a
type instance Str Resolved = Builder
type instance Str Unresolved = Contents

type family Mixins a
type instance Mixins Resolved = ()
type instance Mixins Unresolved = [Deref]

data Block a = Block
    { blockSelector :: !(Selector a)
    , blockAttrs :: ![Attr a]
    , blockBlocks :: !(ChildBlocks a)
    , blockMixins :: !(Mixins a)
    }

data Mixin = Mixin
    { mixinAttrs :: ![Attr Resolved]
    , mixinBlocks :: ![Block Resolved]
    }
instance Monoid Mixin where
    mempty = Mixin mempty mempty
    mappend (Mixin a x) (Mixin b y) = Mixin (a ++ b) (x ++ y)

data TopLevel a where
    TopBlock   :: !(Block a) -> TopLevel a
    TopAtBlock :: !String -- name e.g., media
               -> !(Str a) -- selector
               -> ![Block a]
               -> TopLevel a
    TopAtDecl  :: !String -> !(Str a) -> TopLevel a
    TopVar     :: !String -> !String -> TopLevel Unresolved

data Attr a = Attr
    { attrKey :: !(Str a)
    , attrVal :: !(Str a)
    }

data Css = CssWhitespace ![TopLevel Resolved]
         | CssNoWhitespace ![TopLevel Resolved]

data Content = ContentRaw String
             | ContentVar Deref
             | ContentUrl Deref
             | ContentUrlParam Deref
             | ContentMixin Deref
    deriving (Show, Eq)

type Contents = [Content]

data CDData url = CDPlain Builder
                | CDUrl url
                | CDUrlParam (url, [(Text, Text)])
                | CDMixin Mixin

pack :: String -> Text
pack = T.pack
#if !MIN_VERSION_text(0, 11, 2)
{-# NOINLINE pack #-}
#endif

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
                   -> Parser [TopLevel Unresolved]
                   -> String
                   -> [(Deref, VarType)]
cssUsedIdentifiers toi2b parseBlocks s' =
    concat $ runIdentity $ mapM (getVars scope0) contents
  where
    s = if toi2b then i2b s' else s'
    a = either (error . show) id $ parse parseBlocks s s
    (scope0, contents) = go a

    go :: [TopLevel Unresolved]
       -> (Scope, [Content])
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
    go (TopAtBlock _ _ blocks:rest) =
        (scope1 ++ scope2, rest1 ++ rest2)
      where
        (scope1, rest1) = go (map TopBlock blocks)
        (scope2, rest2) = go rest
    go (TopBlock (Block x y z mixins):rest) =
        (scope1 ++ scope2, rest0 ++ rest1 ++ rest2 ++ restm)
      where
        rest0 = intercalate [ContentRaw ","] x ++ concatMap go' y
        (scope1, rest1) = go (map (TopBlock . snd) z)
        (scope2, rest2) = go rest
        restm = map ContentMixin mixins
    go (TopVar k v:rest) =
        ((k, v):scope, rest')
      where
        (scope, rest') = go rest
    go' (Attr k v) = k ++ v

cssFileDebug :: Bool -- ^ perform the indent-to-brace conversion
             -> Q Exp
             -> Parser [TopLevel Unresolved]
             -> FilePath
             -> Q Exp
cssFileDebug toi2b parseBlocks' parseBlocks fp = do
    s <- fmap TL.unpack $ qRunIO $ readUtf8File fp
#ifdef GHC_7_4
    qAddDependentFile fp
#endif
    let vs = cssUsedIdentifiers toi2b parseBlocks s
    c <- mapM vtToExp vs
    cr <- [|cssRuntime toi2b|]
    parseBlocks'' <- parseBlocks'
    return $ cr `AppE` parseBlocks'' `AppE` (LitE $ StringL fp) `AppE` ListE c

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
             -> Block Unresolved
             -> Either String (DList (Block Resolved))
-- FIXME share code with blockToCss
blockRuntime cd render' (Block x attrs z mixinsDerefs) = do
    mixins <- mapM getMixin mixinsDerefs
    x' <- mapM go' $ intercalate [ContentRaw ","] x
    attrs' <- mapM resolveAttr attrs
    z' <- mapM (subGo x) z -- FIXME use difflists again
    Right $ \rest -> Block
        { blockSelector = mconcat x'
        , blockAttrs    = concat $ attrs' : map mixinAttrs mixins
        , blockBlocks   = ()
        , blockMixins   = ()
        } : foldr ($) rest z'
    {-
    (:) (Css' (mconcat $ map go' $ intercalate [ContentRaw "," ] x) (map go'' y))
    . foldr (.) id (map (subGo x) z)
    -}
  where
    go' = contentToBuilderRT cd render'

    getMixin d =
        case lookup d cd of
            Nothing -> Left $ "Mixin not found: " ++ show d
            Just (CDMixin m) -> Right m
            Just _ -> Left $ "For " ++ show d ++ ", expected Mixin"

    resolveAttr :: Attr Unresolved -> Either String (Attr Resolved)
    resolveAttr (Attr k v) = Attr <$> (mconcat <$> mapM go' k) <*> (mconcat <$> mapM go' v)

    subGo :: [Contents] -- ^ parent selectors
          -> (HasLeadingSpace, Block Unresolved)
          -> Either String (DList (Block Resolved))
    subGo x' (hls, Block a b c d) =
        blockRuntime cd render' (Block a' b c d)
      where
        a' = combineSelectors hls x' a

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
           -> Parser [TopLevel Unresolved]
           -> FilePath
           -> [(Deref, CDData url)]
           -> (url -> [(Text, Text)] -> Text)
           -> Css
cssRuntime toi2b parseBlocks fp cd render' = unsafePerformIO $ do
    s' <- fmap TL.unpack $ qRunIO $ readUtf8File fp
    let s = if toi2b then i2b s' else s'
    let a = either (error . show) id $ parse parseBlocks s s
    return $ CssWhitespace $ goTop [] a
  where
    goTop :: [(String, String)] -- ^ scope
          -> [TopLevel Unresolved]
          -> [TopLevel Resolved]
    goTop _ [] = []
    goTop scope (TopAtDecl dec cs':rest) =
        TopAtDecl dec cs : goTop scope rest
      where
        cs = either error mconcat $ mapM (contentToBuilderRT cd render') cs'
    goTop scope (TopBlock b:rest) =
        map TopBlock (either error ($[]) $ blockRuntime (addScope scope) render' b) ++
        goTop scope rest
    goTop scope (TopAtBlock name s' b:rest) =
        TopAtBlock name s (foldr (either error id . blockRuntime (addScope scope) render') [] b) :
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
    c VTMixin = [|CDMixin|]

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
getVars scope (ContentMixin d) =
    case lookupD d scope of
        Nothing -> return [(d, VTMixin)]
        Just s -> fail $ "Expected Mixin for " ++ s

lookupD :: Deref -> [(String, b)] -> Maybe String
lookupD (DerefIdent (Ident s)) scope =
    case lookup s scope of
        Nothing -> Nothing
        Just _ -> Just s
lookupD _ _ = Nothing

compressTopLevel :: TopLevel Unresolved
                 -> TopLevel Unresolved
compressTopLevel (TopBlock b) = TopBlock $ compressBlock b
compressTopLevel (TopAtBlock name s b) = TopAtBlock name s $ map compressBlock b
compressTopLevel x@TopAtDecl{} = x
compressTopLevel x@TopVar{} = x

compressBlock :: Block Unresolved
              -> Block Unresolved
compressBlock (Block x y blocks mixins) =
    Block (map cc x) (map go y) (map (second compressBlock) blocks) mixins
  where
    go (Attr k v) = Attr (cc k) (cc v)
    cc [] = []
    cc (ContentRaw a:ContentRaw b:c) = cc $ ContentRaw (a ++ b) : c
    cc (a:b) = a : cc b

blockToMixin :: Name
             -> Scope
             -> Block Unresolved
             -> Q Exp
blockToMixin r scope (Block _sel props subblocks mixins) =
    [|Mixin
        { mixinAttrs    = concat
                        $ $(listE $ map go props)
                        : map mixinAttrs $mixinsE
        -- FIXME too many complications to implement sublocks for now...
        , mixinBlocks   = [] -- foldr (.) id $(listE $ map subGo subblocks) []
        }|]
      {-
      . foldr (.) id $(listE $ map subGo subblocks)
      . (concatMap mixinBlocks $mixinsE ++)
    |]
    -}
  where
    mixinsE = return $ ListE $ map (derefToExp []) mixins
    go (Attr x y) = conE 'Attr
        `appE` (contentsToBuilder r scope x)
        `appE` (contentsToBuilder r scope y)
    subGo (Block sel' b c d) = blockToCss r scope $ Block sel' b c d

blockToCss :: Name
           -> Scope
           -> Block Unresolved
           -> Q Exp
blockToCss r scope (Block sel props subblocks mixins) =
    [|((Block
        { blockSelector = $(selectorToBuilder r scope sel)
        , blockAttrs    = concat
                        $ $(listE $ map go props)
                        : map mixinAttrs $mixinsE
        , blockBlocks   = ()
        , blockMixins   = ()
        } :: Block Resolved):)
      . foldr (.) id $(listE $ map subGo subblocks)
      . (concatMap mixinBlocks $mixinsE ++)
    |]
  where
    mixinsE = return $ ListE $ map (derefToExp []) mixins
    go (Attr x y) = conE 'Attr
        `appE` (contentsToBuilder r scope x)
        `appE` (contentsToBuilder r scope y)
    subGo (hls, Block sel' b c d) =
        blockToCss r scope $ Block sel'' b c d
      where
        sel'' = combineSelectors hls sel sel'

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

topLevelsToCassius :: [TopLevel Unresolved]
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
    go r scope (TopAtBlock name s b:rest) = do
        let s' = contentsToBuilder r scope s
        e <- [|(:) $ TopAtBlock $(lift name) $(s') $(blocksToCassius r scope b)|]
        es <- go r scope rest
        return $ e : es
    go r scope (TopAtDecl dec cs:rest) = do
        e <- [|(:) $ TopAtDecl $(lift dec) $(contentsToBuilder r scope cs)|]
        es <- go r scope rest
        return $ e : es
    go r scope (TopVar k v:rest) = go r ((k, v) : scope) rest

blocksToCassius :: Name
                -> Scope
                -> [Block Unresolved]
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
    go (TopBlock x) = renderBlock haveWhiteSpace mempty x
    go (TopAtBlock name s x) =
        fromText (pack $ concat ["@", name, " "]) `mappend`
        s `mappend`
        startBlock `mappend`
        foldr mappend endBlock (map (renderBlock haveWhiteSpace (fromString "    ")) x)
    go (TopAtDecl dec cs) = fromText (pack $ concat ["@", dec, " "]) `mappend`
                      cs `mappend`
                      endDecl

    startBlock
        | haveWhiteSpace = fromString " {\n"
        | otherwise = singleton '{'

    endBlock
        | haveWhiteSpace = fromString "}\n"
        | otherwise = singleton '}'

    endDecl
        | haveWhiteSpace = fromString ";\n"
        | otherwise = singleton ';'

renderBlock :: Bool -- ^ have whitespace?
            -> Builder -- ^ indentation
            -> Block Resolved
            -> Builder
renderBlock haveWhiteSpace indent (Block sel attrs () ())
    | null attrs = mempty
    | otherwise = startSelect
               <> sel
               <> startBlock
               <> mconcat (intersperse endDecl $ map renderAttr attrs)
               <> endBlock
  where
    renderAttr (Attr k v) = startDecl <> k <> colon <> v

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

instance Lift Mixin where
    lift (Mixin a b) = [|Mixin a b|]
instance Lift (Attr Unresolved) where
    lift (Attr k v) = [|Attr k v :: Attr Unresolved |]
instance Lift (Attr Resolved) where
    lift (Attr k v) = [|Attr $(liftBuilder k) $(liftBuilder v) :: Attr Resolved |]

liftBuilder :: Builder -> Q Exp
liftBuilder b = [|fromText $ pack $(lift $ TL.unpack $ toLazyText b)|]

instance Lift Content where
    lift (ContentRaw s) = [|ContentRaw s|]
    lift (ContentVar d) = [|ContentVar d|]
    lift (ContentUrl d) = [|ContentUrl d|]
    lift (ContentUrlParam d) = [|ContentUrlParam d|]
    lift (ContentMixin m) = [|ContentMixin m|]
instance Lift (Block Unresolved) where
    lift (Block a b c d) = [|Block a b c d|]
instance Lift (Block Resolved) where
    lift (Block a b () ()) = [|Block $(liftBuilder a) b () ()|]
