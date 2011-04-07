{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-- | For lack of a better name... a parameterized version of Julius.
module Text.Romeo
    ( RomeoSettings (..)
    , romeo
    , romeoFile
    , romeoFileDebug
    ) where

import Text.ParserCombinators.Parsec hiding (Line)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Syntax.Internals
import Data.Text.Lazy.Builder (Builder, fromText)
import Data.Monoid
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Text.Hamlet.Quasi (readUtf8File)
import Text.Shakespeare

data RomeoSettings = RomeoSettings
    { varChar :: Char
    , urlChar :: Char
    , intChar :: Char
    , toBuilder :: Exp
    , wrap :: Exp
    , unwrap :: Exp
    }

instance Lift RomeoSettings where
    lift (RomeoSettings x1 x2 x3 x4 x5 x6) =
        [|RomeoSettings
            $(lift x1) $(lift x2) $(lift x3)
            $(liftExp x4) $(liftExp x5) $(liftExp x6)|]
      where
        liftExp (VarE n) = [|VarE $(liftName n)|]
        liftExp (ConE n) = [|ConE $(liftName n)|]
        liftExp _ = error "liftExp only supports VarE and ConE"
        liftName (Name (OccName a) b) = [|Name (OccName $(lift a)) $(liftFlavour b)|]
        liftFlavour NameS = [|NameS|]
        liftFlavour (NameQ (ModName a)) = [|NameQ (ModName $(lift a))|]
        liftFlavour (NameU _) = error "liftFlavour NameU" -- [|NameU $(lift $ fromIntegral a)|]
        liftFlavour (NameL _) = error "liftFlavour NameL" -- [|NameU $(lift $ fromIntegral a)|]
        liftFlavour (NameG ns (PkgName p) (ModName m)) = [|NameG $(liftNS ns) (PkgName $(lift p)) (ModName $(lift m))|]
        liftNS VarName = [|VarName|]
        liftNS DataName = [|DataName|]

type Romeo url = (url -> [(TS.Text, TS.Text)] -> TS.Text) -> Builder

data Content = ContentRaw String
             | ContentVar Deref
             | ContentUrl Deref
             | ContentUrlParam Deref
             | ContentMix Deref
    deriving (Show, Eq)
type Contents = [Content]

parseContents :: RomeoSettings -> Parser Contents
parseContents = many1 . parseContent

parseContent :: RomeoSettings -> Parser Content
parseContent RomeoSettings {..} =
    parseHash' <|> parseAt' <|> parseCaret' <|> parseChar
  where
    parseHash' = either ContentRaw ContentVar `fmap` parseVar varChar
    parseAt' =
        either ContentRaw go `fmap` parseUrl urlChar '?'
      where
        go (d, False) = ContentUrl d
        go (d, True) = ContentUrlParam d
    parseCaret' = either ContentRaw ContentMix `fmap` parseInt intChar
    parseChar = ContentRaw `fmap` (many1 $ noneOf [varChar, urlChar, intChar])

compressContents :: Contents -> Contents
compressContents [] = []
compressContents (ContentRaw x:ContentRaw y:z) =
    compressContents $ ContentRaw (x ++ y) : z
compressContents (x:y) = x : compressContents y

contentsToRomeo :: RomeoSettings -> [Content] -> Q Exp
contentsToRomeo rs a = do
    r <- newName "_render"
    c <- mapM (contentToBuilder rs r) a
    d <- case c of
            [] -> [|mempty|]
            [x] -> return x
            _ -> do
                mc <- [|mconcat|]
                return $ mc `AppE` ListE c
    return $ LamE [VarP r] $ wrap rs `AppE` d

romeo :: RomeoSettings -> QuasiQuoter
romeo r = QuasiQuoter { quoteExp = romeoFromString r }

romeoFromString :: RomeoSettings -> String -> Q Exp
romeoFromString r s = do
    let a = either (error . show) id $ parse (parseContents r) s s
    contentsToRomeo r $ compressContents a

contentToBuilder :: RomeoSettings -> Name -> Content -> Q Exp
contentToBuilder _ _ (ContentRaw s') = do
    ts <- [|fromText . TS.pack|]
    return $ ts `AppE` LitE (StringL s')
contentToBuilder rs _ (ContentVar d) = do
    return $ toBuilder rs `AppE` derefToExp [] d
contentToBuilder _ r (ContentUrl d) = do
    ts <- [|fromText|]
    return $ ts `AppE` (VarE r `AppE` derefToExp [] d `AppE` ListE [])
contentToBuilder _ r (ContentUrlParam d) = do
    ts <- [|fromText|]
    up <- [|\r' (u, p) -> r' u p|]
    return $ ts `AppE` (up `AppE` VarE r `AppE` derefToExp [] d)
contentToBuilder rs r (ContentMix d) = do
    return $ unwrap rs `AppE` (derefToExp [] d `AppE` VarE r)

romeoFile :: RomeoSettings -> FilePath -> Q Exp
romeoFile r fp = do
    contents <- qRunIO $ readUtf8File fp
    romeoFromString r $ TL.unpack contents

data VarType = VTPlain | VTUrl | VTUrlParam | VTMixin

getVars :: Content -> [(Deref, VarType)]
getVars ContentRaw{} = []
getVars (ContentVar d) = [(d, VTPlain)]
getVars (ContentUrl d) = [(d, VTUrl)]
getVars (ContentUrlParam d) = [(d, VTUrlParam)]
getVars (ContentMix d) = [(d, VTMixin)]

data JDData url = JDPlain Builder
                | JDUrl url
                | JDUrlParam (url, [(TS.Text, TS.Text)])
                | JDMixin (Romeo url)

vtToExp :: RomeoSettings -> (Deref, VarType) -> Q Exp
vtToExp rs (d, vt) = do
    d' <- lift d
    c' <- c vt
    return $ TupE [d', c' `AppE` derefToExp [] d]
  where
    c :: VarType -> Q Exp
    c VTPlain = [|JDPlain . $(return $ toBuilder rs)|]
    c VTUrl = [|JDUrl|]
    c VTUrlParam = [|JDUrlParam|]
    c VTMixin = [|\x -> JDMixin $ \r -> $(return $ unwrap rs) $ x r|]

romeoFileDebug :: RomeoSettings -> FilePath -> Q Exp
romeoFileDebug r fp = do
    s <- qRunIO $ fmap TL.unpack $ readUtf8File fp
    let a = either (error . show) id $ parse (parseContents r) s s
        b = concatMap getVars a
    c <- mapM (vtToExp r) b
    cr <- [|romeoRuntime|]
    wrap' <- [|\x -> $(return $ wrap r) . x|]
    r' <- lift r
    return $ wrap' `AppE` (cr `AppE` r' `AppE` (LitE $ StringL fp) `AppE` ListE c)

romeoRuntime :: RomeoSettings -> FilePath -> [(Deref, JDData url)] -> Romeo url
romeoRuntime r fp cd render' = unsafePerformIO $ do
    s <- fmap TL.unpack $ readUtf8File fp
    let a = either (error . show) id $ parse (parseContents r) s s
    return $ mconcat $ map go a
  where
    go :: Content -> Builder
    go (ContentRaw s) = fromText $ TS.pack s
    go (ContentVar d) =
        case lookup d cd of
            Just (JDPlain s) -> s
            _ -> error $ show d ++ ": expected JDPlain"
    go (ContentUrl d) =
        case lookup d cd of
            Just (JDUrl u) -> fromText $ render' u []
            _ -> error $ show d ++ ": expected JDUrl"
    go (ContentUrlParam d) =
        case lookup d cd of
            Just (JDUrlParam (u, p)) ->
                fromText $ render' u p
            _ -> error $ show d ++ ": expected JDUrlParam"
    go (ContentMix d) =
        case lookup d cd of
            Just (JDMixin m) -> m render'
            _ -> error $ show d ++ ": expected JDMixin"
