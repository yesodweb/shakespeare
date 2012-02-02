{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-- | For lack of a better name... a parameterized version of Julius.
module Text.Shakespeare
    ( ShakespeareSettings (..)
    , PreConvert (..)
    , PreConversion (..)
    , defaultShakespeareSettings
    , shakespeare
    , shakespeareFile
    , shakespeareFileDebug
    -- * low-level
    , shakespeareFromString
    , RenderUrl

#ifdef TEST_EXPORT
    , preFilter
#endif
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
import Text.Shakespeare.Base

-- for pre conversion
import System.Process (readProcess)

-- move to Shakespeare.Base?
readFileQ :: FilePath -> Q String
readFileQ fp = qRunIO $ readFileUtf8 fp

-- move to Shakespeare.Base?
readFileUtf8 :: FilePath -> IO String
readFileUtf8 fp = fmap TL.unpack $ readUtf8File fp

-- | The Coffeescript language compiles down to Javascript.
-- Previously we waited until the very end, at the rendering stage to perform this compilation.
-- Lets call is a post-conversion
-- This had the advantage that all Haskell values were inserted first:
-- for example a value could be inserted that Coffeescript would compile into Javascript.
-- While that is perhaps a safer approach, the advantage is not used in practice:
-- it was that way mainly for ease of implementation.
-- The down-side is the template must be compiled down to Javascript during every request.
-- If instead we do a pre-conversion to compile down to Javascript,
-- we only need to perform the compilation once.
-- During the pre-conversion we first modify all Haskell insertions
-- so that they will be ignored by the Coffeescript compiler (backticks).
-- So %{var} is change to `%{var}` using the preEscapeBegin and preEscapeEnd.
-- preEscapeIgnore is used to not insert backtacks for variable already inside strings - coffeescript will happily ignore them, and won't treat backticks as escaping

data PreConvert = PreConvert
    { preConvert :: PreConversion
    , preEscapeBegin :: String
    , preEscapeEnd   :: String
    , preEscapeIgnore :: [Char]
    }

data PreConversion = ReadProcess String [String]
                   | Id
  


data ShakespeareSettings = ShakespeareSettings
    { varChar :: Char
    , urlChar :: Char
    , intChar :: Char
    , toBuilder :: Exp
    , wrap :: Exp
    , unwrap :: Exp
    , justVarInterpolation :: Bool
    , preConversion :: Maybe PreConvert
    }

defaultShakespeareSettings :: ShakespeareSettings
defaultShakespeareSettings = ShakespeareSettings {
    varChar = '#'
  , urlChar = '@'
  , intChar = '^'
  , justVarInterpolation = False
  , preConversion = Nothing
}

instance Lift PreConvert where
    lift (PreConvert convert begin end ignore) =
        [|PreConvert $(lift convert) $(lift begin) $(lift end) $(lift ignore)|]

instance Lift PreConversion where
    lift (ReadProcess command args) =
        [|ReadProcess $(lift command) $(lift args)|]
    lift Id = [|Id|]

instance Lift ShakespeareSettings where
    lift (ShakespeareSettings x1 x2 x3 x4 x5 x6 x7 x8) =
        [|ShakespeareSettings
            $(lift x1) $(lift x2) $(lift x3)
            $(liftExp x4) $(liftExp x5) $(liftExp x6) $(lift x7) $(lift x8)|]
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

type QueryParameters = [(TS.Text, TS.Text)]
type RenderUrl url = (url -> QueryParameters -> TS.Text)
type Shakespeare url = RenderUrl url -> Builder

data Content = ContentRaw String
             | ContentVar Deref
             | ContentUrl Deref
             | ContentUrlParam Deref
             | ContentMix Deref
    deriving (Show, Eq)
type Contents = [Content]

eShowErrors :: Either ParseError c -> c
eShowErrors = either (error . show) id

contentFromString :: ShakespeareSettings -> String -> [Content]
contentFromString rs s =
    compressContents $ eShowErrors $ parse (parseContents rs) s s
  where
    compressContents :: Contents -> Contents
    compressContents [] = []
    compressContents (ContentRaw x:ContentRaw y:z) =
        compressContents $ ContentRaw (x ++ y) : z
    compressContents (x:y) = x : compressContents y
    
parseContents :: ShakespeareSettings -> Parser Contents
parseContents = many1 . parseContent
  where
    parseContent :: ShakespeareSettings -> Parser Content
    parseContent ShakespeareSettings {..} =
        parseVar' <|> parseUrl' <|> parseInt' <|> parseChar'
      where
        parseVar' = either ContentRaw ContentVar `fmap` parseVar varChar
        parseUrl' = either ContentRaw contentUrl `fmap` parseUrl urlChar '?'
          where
            contentUrl (d, False) = ContentUrl d
            contentUrl (d, True) = ContentUrlParam d

        parseInt' = either ContentRaw ContentMix `fmap` parseInt intChar
        parseChar' = ContentRaw `fmap` many1 (noneOf [varChar, urlChar, intChar])


preFilter :: ShakespeareSettings -> String -> IO String
preFilter ShakespeareSettings {..} s = 
    case preConversion of
      Nothing -> return s
      Just pre@(PreConvert convert _ _ _) ->
        let parsed = mconcat $ eShowErrors $ parse (parseConvert pre) s s
        in  case convert of
              Id -> return parsed
              ReadProcess command args ->
                readProcess command (args ++ [parsed]) []
  where
    parseConvert PreConvert {..} = many1 $ choice $
        (map (try . escapedParse) preEscapeIgnore) ++
        [mainParser preEscapeIgnore]

      where
        escapedParse ignoreC = do
            _<- char ignoreC
            inside <- many $ noneOf [ignoreC]
            _<- char ignoreC
            return $ ignoreC:inside ++ [ignoreC]
            -- return $ ignoreC:(eShowErrors $ parse (mainParser escapeNone) "" inside) ++ [ignoreC]

        mainParser i = parseVar' <|> parseUrl' <|> parseInt' <|> parseChar' i
        escape str = preEscapeBegin ++ str ++ preEscapeEnd
        escapeRight = either id escape

        parseVar' = escapeRight `fmap` parseVarString varChar
        parseUrl' = escapeRight `fmap` parseUrlString urlChar '?'
        parseInt' = escapeRight `fmap` parseIntString intChar
        parseChar' i = many1 (noneOf ([varChar, urlChar, intChar] ++ i))


contentsToShakespeare :: ShakespeareSettings -> [Content] -> Q Exp
contentsToShakespeare rs a = do
    r <- newName "_render"
    c <- mapM (contentToBuilder r) a
    compiledTemplate <- case c of
        []  -> [|mempty|]
        [x] -> return x
        _   -> do
              mc <- [|mconcat|]
              return $ mc `AppE` ListE c
    if justVarInterpolation rs
      then return compiledTemplate
      else return $ LamE [VarP r] compiledTemplate
      where
        contentToBuilder :: Name -> Content -> Q Exp
        contentToBuilder _ (ContentRaw s') = do
            ts <- [|fromText . TS.pack|]
            return $ wrap rs `AppE` (ts `AppE` LitE (StringL s'))
        contentToBuilder _ (ContentVar d) = do
            return $ wrap rs `AppE` (toBuilder rs `AppE` derefToExp [] d)
        contentToBuilder r (ContentUrl d) = do
            ts <- [|fromText|]
            return $ wrap rs `AppE` (ts `AppE` (VarE r `AppE` derefToExp [] d `AppE` ListE []))
        contentToBuilder r (ContentUrlParam d) = do
            ts <- [|fromText|]
            up <- [|\r' (u, p) -> r' u p|]
            return $ wrap rs `AppE` (ts `AppE` (up `AppE` VarE r `AppE` derefToExp [] d))
        contentToBuilder r (ContentMix d) = do
            return $ derefToExp [] d `AppE` VarE r

shakespeare :: ShakespeareSettings -> QuasiQuoter
shakespeare r = QuasiQuoter { quoteExp = shakespeareFromString r }

shakespeareFromString :: ShakespeareSettings -> String -> Q Exp
shakespeareFromString r str = do
    s <- qRunIO $ preFilter r str
    contentsToShakespeare r $ contentFromString r $ s

shakespeareFile :: ShakespeareSettings -> FilePath -> Q Exp
shakespeareFile r fp = readFileQ fp >>= shakespeareFromString r

data VarType = VTPlain | VTUrl | VTUrlParam | VTMixin

getVars :: Content -> [(Deref, VarType)]
getVars ContentRaw{} = []
getVars (ContentVar d) = [(d, VTPlain)]
getVars (ContentUrl d) = [(d, VTUrl)]
getVars (ContentUrlParam d) = [(d, VTUrlParam)]
getVars (ContentMix d) = [(d, VTMixin)]

data VarExp url = EPlain Builder
                | EUrl url
                | EUrlParam (url, [(TS.Text, TS.Text)])
                | EMixin (Shakespeare url)

shakespeareFileDebug :: ShakespeareSettings -> FilePath -> Q Exp
shakespeareFileDebug rs fp = do
    str <- readFileQ fp
    s <- qRunIO $ preFilter rs str
    let b = concatMap getVars $ contentFromString rs s
    c <- mapM vtToExp b
    rt <- [|shakespeareRuntime|]
    wrap' <- [|\x -> $(return $ wrap rs) . x|]
    r' <- lift rs
    return $ wrap' `AppE` (rt `AppE` r' `AppE` (LitE $ StringL fp) `AppE` ListE c)
  where
    vtToExp :: (Deref, VarType) -> Q Exp
    vtToExp (d, vt) = do
        d' <- lift d
        c' <- c vt
        return $ TupE [d', c' `AppE` derefToExp [] d]
      where
        c :: VarType -> Q Exp
        c VTPlain = [|EPlain . $(return $ toBuilder rs)|]
        c VTUrl = [|EUrl|]
        c VTUrlParam = [|EUrlParam|]
        c VTMixin = [|\x -> EMixin $ \r -> $(return $ unwrap rs) $ x r|]


shakespeareRuntime :: ShakespeareSettings -> FilePath -> [(Deref, VarExp url)] -> Shakespeare url
shakespeareRuntime rs fp cd render' = unsafePerformIO $ do
    str <- readFileUtf8 fp
    s <- preFilter rs str
    return $ mconcat $ map go $ contentFromString rs s
  where
    go :: Content -> Builder
    go (ContentRaw s) = fromText $ TS.pack s
    go (ContentVar d) =
        case lookup d cd of
            Just (EPlain s) -> s
            _ -> error $ show d ++ ": expected EPlain"
    go (ContentUrl d) =
        case lookup d cd of
            Just (EUrl u) -> fromText $ render' u []
            _ -> error $ show d ++ ": expected EUrl"
    go (ContentUrlParam d) =
        case lookup d cd of
            Just (EUrlParam (u, p)) ->
                fromText $ render' u p
            _ -> error $ show d ++ ": expected EUrlParam"
    go (ContentMix d) =
        case lookup d cd of
            Just (EMixin m) -> m render'
            _ -> error $ show d ++ ": expected EMixin"
