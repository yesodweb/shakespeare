{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
-- | NOTE: This module should be considered internal, and will be hidden in
-- future releases.
module Text.Shakespeare
    ( ShakespeareSettings (..)
    , PreConvert (..)
    , WrapInsertion (..)
    , PreConversion (..)
    , defaultShakespeareSettings
    , shakespeare
    , shakespeareFile
    , shakespeareFileReload
    -- * low-level
    , shakespeareFromString
    , shakespeareUsedIdentifiers
    , RenderUrl
    , VarType (..)
    , Deref
    , Parser

    , preFilter
      -- * Internal
      -- can we remove this?
    , shakespeareRuntime
    , pack'
    ) where

import Data.List (intersperse)
import Data.Char (isAlphaNum, isSpace)
import Text.ParserCombinators.Parsec hiding (Line, parse, Parser)
import Text.Parsec.Prim (modifyState, Parsec)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH (appE)
import Language.Haskell.TH.Syntax
#if !MIN_VERSION_template_haskell(2,8,0)
import Language.Haskell.TH.Syntax.Internals
#endif
import Data.Text.Lazy.Builder (Builder, fromText)
import Data.Monoid
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Text.Shakespeare.Base

import System.Directory (getModificationTime)
import Data.Time (UTCTime)
import Data.IORef
import qualified Data.Map as M
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Data (Data)

-- for pre conversion
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

#if !MIN_VERSION_base(4,5,0)
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}
#endif

-- | A parser with a user state of [String]
type Parser = Parsec String [String]
-- | run a parser with a user state of [String]
parse ::  GenParser tok [a1] a -> SourceName -> [tok] -> Either ParseError a
parse p = runParser p []

-- move to Shakespeare.Base?
readFileQ :: FilePath -> Q String
readFileQ fp = qRunIO $ readFileUtf8 fp

-- move to Shakespeare.Base?
readFileUtf8 :: FilePath -> IO String
readFileUtf8 fp = fmap TL.unpack $ readUtf8File fp

-- | Coffeescript, TypeScript, and other languages compiles down to Javascript.
-- Previously we waited until the very end, at the rendering stage to perform this compilation.
-- Lets call is a post-conversion
-- This had the advantage that all Haskell values were inserted first:
-- for example a value could be inserted that Coffeescript would compile into Javascript.
-- While that is perhaps a safer approach, the advantage is not used in practice:
-- it was that way mainly for ease of implementation.
-- The down-side is the template must be compiled down to Javascript during every request.
-- If instead we do a pre-conversion to compile down to Javascript,
-- we only need to perform the compilation once.
--
-- The problem then is the insertion of Haskell values: we need a hole for
-- them. This can be done with variables known to the language.
-- During the pre-conversion we first modify all Haskell insertions
-- So #{a} is change to shakespeare_var_a
-- Then we can place the Haskell values in a function wrapper that exposes
-- those variables: (function(shakespeare_var_a){ ... shakespeare_var_a ...})
-- TypeScript can compile that, and then we tack an application of the
-- Haskell values onto the result: (#{a})
--
-- preEscapeIgnoreBalanced is used to not insert backtacks for variable already inside strings or backticks.
-- coffeescript will happily ignore the interpolations, and backticks would not be treated as escaping in that context.
-- preEscapeIgnoreLine was added to ignore comments (which in Coffeescript begin with a '#')

data PreConvert = PreConvert
    { preConvert :: PreConversion
    , preEscapeIgnoreBalanced :: [Char]
    , preEscapeIgnoreLine :: [Char]
    , wrapInsertion :: Maybe WrapInsertion
    }

data WrapInsertion = WrapInsertion {
      wrapInsertionIndent     :: Maybe String
    , wrapInsertionStartBegin :: String
    , wrapInsertionSeparator  :: String
    , wrapInsertionStartClose :: String
    , wrapInsertionEnd :: String
    , wrapInsertionAddParens :: Bool
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
    , modifyFinalValue :: Maybe Exp
    -- ^ A transformation applied to the final expression. Most often, this
    -- would be used to force the type of the expression to help make more
    -- meaningful error messages.
    }

defaultShakespeareSettings
    :: Exp -- ^ An Exp that will convert variables to 'Builder'
    -> Exp -- ^ An Exp that converts 'Builder' to the type being generated
    -> Exp -- ^ The reversal of the previous conversion argument
    -> ShakespeareSettings
defaultShakespeareSettings tobuild wrapexp unwrapexp = ShakespeareSettings {
    varChar = '#'
  , urlChar = '@'
  , intChar = '^'
  , toBuilder = tobuild
  , wrap = wrapexp
  , unwrap = unwrapexp
  , justVarInterpolation = False
  , preConversion = Nothing
  , modifyFinalValue = Nothing
}

instance Lift PreConvert where
    lift (PreConvert convert ignore comment wrapInsertion) =
        [|PreConvert $(lift convert) $(lift ignore) $(lift comment) $(lift wrapInsertion)|]

instance Lift WrapInsertion where
    lift (WrapInsertion indent sb sep sc e wp) =
        [|WrapInsertion $(lift indent) $(lift sb) $(lift sep) $(lift sc) $(lift e) $(lift wp)|]

instance Lift PreConversion where
    lift (ReadProcess command args) =
        [|ReadProcess $(lift command) $(lift args)|]
    lift Id = [|Id|]

instance Lift ShakespeareSettings where
    lift (ShakespeareSettings x1 x2 x3 x4 x5 x6 x7 x8 x9) =
        [|ShakespeareSettings
            $(lift x1) $(lift x2) $(lift x3)
            $(liftExp x4) $(liftExp x5) $(liftExp x6) $(lift x7) $(lift x8) $(liftMExp x9)|]
      where
        liftExp (VarE n) = [|VarE $(liftName n)|]
        liftExp (ConE n) = [|ConE $(liftName n)|]
        liftExp _ = error "liftExp only supports VarE and ConE"
        liftMExp Nothing = [|Nothing|]
        liftMExp (Just e) = [|Just|] `appE` liftExp e
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
contentFromString _ "" = []
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


-- | calls 'error' when there is stderr or exit code failure
readProcessError :: FilePath -> [String] -> String
                 -> Maybe FilePath -- ^ for error reporting
                 -> IO String
readProcessError cmd args input mfp = do
  (ex, output, err) <- readProcessWithExitCode cmd args input
  case ex of
   ExitSuccess   ->
     case err of
       [] -> return output
       msg -> error $ "stderr received during readProcess:" ++ displayCmd ++ "\n\n" ++ msg
   ExitFailure r ->
    error $ "exit code " ++ show r ++ " from readProcess: " ++ displayCmd ++ "\n\n"
      ++ "stderr:\n" ++ err
  where
    displayCmd = cmd ++ ' ':unwords (map show args) ++
        case mfp of
          Nothing -> ""
          Just fp -> ' ':fp

preFilter :: Maybe FilePath -- ^ for error reporting
          -> ShakespeareSettings
          -> String
          -> IO String
preFilter mfp ShakespeareSettings {..} template =
    case preConversion of
      Nothing -> return template
      Just pre@(PreConvert convert _ _ mWrapI) ->
        if all isSpace template then return template else
          let (groups, rvars) = eShowErrors $ parse
                                  (parseConvertWrapInsertion mWrapI pre)
                                  template
                                  template
              vars = reverse rvars
              parsed = mconcat groups
              withVars = (addVars mWrapI vars parsed)
          in  applyVars mWrapI vars `fmap` case convert of
                  Id -> return withVars
                  ReadProcess command args ->
                    readProcessError command args withVars mfp
  where
    addIndent :: Maybe String -> String -> String
    addIndent Nothing str = str
    addIndent (Just indent) str = mapLines (\line -> indent <> line) str
      where
        mapLines f = unlines . map f . lines

    shakespeare_prefix = "shakespeare_var_"
    shakespeare_var_conversion ('@':'?':'{':str) = shakespeare_var_conversion ('@':'{':str)
    shakespeare_var_conversion (_:'{':str) = shakespeare_prefix <> filter isAlphaNum (init str)
    shakespeare_var_conversion err = error $ "did not expect: " <> err

    applyVars _      [] str = str
    applyVars Nothing _ str = str
    applyVars (Just WrapInsertion {..}) vars str =
         (if wrapInsertionAddParens then "(" else "")
      <> removeTrailingSemiColon
      <> (if wrapInsertionAddParens then ")" else "")
      <> "("
      <> mconcat (intersperse ", " vars)
      <> ");\n"
        where 
          removeTrailingSemiColon = reverse $
             dropWhile (\c -> c == ';' || isSpace c) (reverse str)

    addVars _      [] str = str
    addVars Nothing _ str = str
    addVars (Just WrapInsertion {..}) vars str =
         wrapInsertionStartBegin
      <> mconcat (intersperse wrapInsertionSeparator $ map shakespeare_var_conversion vars)
      <> wrapInsertionStartClose
      <> addIndent wrapInsertionIndent str
      <> wrapInsertionEnd

    parseConvertWrapInsertion Nothing = parseConvert id
    parseConvertWrapInsertion (Just _) = parseConvert shakespeare_var_conversion

    parseConvert varConvert PreConvert {..} = do
        str <- many1 $ choice $
          map (try . escapedParse) preEscapeIgnoreBalanced ++ [mainParser]
        st <- getState
        return (str, st)

      where
        escapedParse ignoreC = do
            _<- char ignoreC
            inside <- many $ noneOf [ignoreC]
            _<- char ignoreC
            return $ ignoreC:inside ++ [ignoreC]

        mainParser =
            parseVar' <|>
            parseUrl' <|>
            parseInt' <|>
            parseCommentLine preEscapeIgnoreLine <|>
            parseChar' preEscapeIgnoreLine preEscapeIgnoreBalanced

        recordRight (Left str)  = return str
        recordRight (Right str) = modifyState (\vars -> str:vars) >> return (varConvert str)

        newLine = "\r\n"
        parseCommentLine cs = do
          begin <- oneOf cs
          comment <- many $ noneOf newLine
          return $ begin : comment

        parseVar' :: (Parsec String [String]) String
        parseVar' = recordRight =<< parseVarString varChar
        parseUrl' = recordRight =<< parseUrlString urlChar '?'
        parseInt' = recordRight =<< parseIntString intChar
        parseChar' comments ignores =
            many1 (noneOf ([varChar, urlChar, intChar] ++ comments ++ ignores))

pack' :: String -> TS.Text
pack' = TS.pack
#if !MIN_VERSION_text(0, 11, 2)
{-# NOINLINE pack' #-}
#endif

contentsToShakespeare :: ShakespeareSettings -> [Content] -> Q Exp
contentsToShakespeare rs a = do
    r <- newName "_render"
    c <- mapM (contentToBuilder r) a
    compiledTemplate <- case c of
        -- Make sure we convert this mempty using toBuilder to pin down the
        -- type appropriately
        []  -> fmap (AppE $ wrap rs) [|mempty|]
        [x] -> return x
        _   -> do
              mc <- [|mconcat|]
              return $ mc `AppE` ListE c
    fmap (maybe id AppE $ modifyFinalValue rs) $ return $
        if justVarInterpolation rs
            then compiledTemplate
            else LamE [VarP r] compiledTemplate
      where
        contentToBuilder :: Name -> Content -> Q Exp
        contentToBuilder _ (ContentRaw s') = do
            ts <- [|fromText . pack'|]
            return $ wrap rs `AppE` (ts `AppE` LitE (StringL s'))
        contentToBuilder _ (ContentVar d) =
            return (toBuilder rs `AppE` derefToExp [] d)
        contentToBuilder r (ContentUrl d) = do
            ts <- [|fromText|]
            return $ wrap rs `AppE` (ts `AppE` (VarE r `AppE` derefToExp [] d `AppE` ListE []))
        contentToBuilder r (ContentUrlParam d) = do
            ts <- [|fromText|]
            up <- [|\r' (u, p) -> r' u p|]
            return $ wrap rs `AppE` (ts `AppE` (up `AppE` VarE r `AppE` derefToExp [] d))
        contentToBuilder r (ContentMix d) =
            return $ derefToExp [] d `AppE` VarE r

shakespeare :: ShakespeareSettings -> QuasiQuoter
shakespeare r = QuasiQuoter { quoteExp = shakespeareFromString r }

shakespeareFromString :: ShakespeareSettings -> String -> Q Exp
shakespeareFromString r str = do
    s <- qRunIO $ preFilter Nothing r $
#ifdef WINDOWS
          filter (/='\r')
#endif
          str
    contentsToShakespeare r $ contentFromString r s

shakespeareFile :: ShakespeareSettings -> FilePath -> Q Exp
shakespeareFile r fp =
#ifdef GHC_7_4
    qAddDependentFile fp >>
#endif
        readFileQ fp >>= shakespeareFromString r

data VarType = VTPlain | VTUrl | VTUrlParam | VTMixin
    deriving (Show, Eq, Ord, Enum, Bounded, Typeable, Data, Generic)

getVars :: Content -> [(Deref, VarType)]
getVars ContentRaw{} = []
getVars (ContentVar d) = [(d, VTPlain)]
getVars (ContentUrl d) = [(d, VTUrl)]
getVars (ContentUrlParam d) = [(d, VTUrlParam)]
getVars (ContentMix d) = [(d, VTMixin)]

data VarExp url = EPlain Builder
                | EUrl url
                | EUrlParam (url, QueryParameters)
                | EMixin (Shakespeare url)

-- | Determine which identifiers are used by the given template, useful for
-- creating systems like yesod devel.
shakespeareUsedIdentifiers :: ShakespeareSettings -> String -> [(Deref, VarType)]
shakespeareUsedIdentifiers settings = concatMap getVars . contentFromString settings

type MTime = UTCTime

{-# NOINLINE reloadMapRef #-}
reloadMapRef :: IORef (M.Map FilePath (MTime, [Content]))
reloadMapRef = unsafePerformIO $ newIORef M.empty

lookupReloadMap :: FilePath -> IO (Maybe (MTime, [Content]))
lookupReloadMap fp = do
  reloads <- readIORef reloadMapRef
  return $ M.lookup fp reloads

insertReloadMap :: FilePath -> (MTime, [Content]) -> IO [Content]
insertReloadMap fp (mt, content) = atomicModifyIORef reloadMapRef
  (\reloadMap -> (M.insert fp (mt, content) reloadMap, content))

shakespeareFileReload :: ShakespeareSettings -> FilePath -> Q Exp
shakespeareFileReload settings fp = do
    str <- readFileQ fp
    s <- qRunIO $ preFilter (Just fp) settings str
    let b = shakespeareUsedIdentifiers settings s
    c <- mapM vtToExp b
    rt <- [|shakespeareRuntime settings fp|]
    wrap' <- [|\x -> $(return $ wrap settings) . x|]
    return $ wrap' `AppE` (rt `AppE` ListE c)
  where
    vtToExp :: (Deref, VarType) -> Q Exp
    vtToExp (d, vt) = do
        d' <- lift d
        c' <- c vt
        return $ TupE [d', c' `AppE` derefToExp [] d]
      where
        c :: VarType -> Q Exp
        c VTPlain = [|EPlain . $(return $
          InfixE (Just $ unwrap settings) (VarE '(.)) (Just $ toBuilder settings))|]
        c VTUrl = [|EUrl|]
        c VTUrlParam = [|EUrlParam|]
        c VTMixin = [|\x -> EMixin $ \r -> $(return $ unwrap settings) $ x r|]



nothingError :: Show a => String -> a -> b
nothingError expected d = error $ "expected " ++ expected ++ " but got Nothing for: " ++ show d

shakespeareRuntime :: ShakespeareSettings -> FilePath -> [(Deref, VarExp url)] -> Shakespeare url
shakespeareRuntime settings fp cd render' = unsafePerformIO $ do
    mtime <- qRunIO $ getModificationTime fp
    mdata <- lookupReloadMap fp
    case mdata of
      Just (lastMtime, lastContents) ->
        if mtime == lastMtime then return $ go' lastContents
          else fmap go' $ newContent mtime
      Nothing -> fmap go' $ newContent mtime
  where
    newContent mtime = do
        str <- readFileUtf8 fp
        s <- preFilter (Just fp) settings str
        insertReloadMap fp (mtime, contentFromString settings s)

    go' = mconcat . map go

    go :: Content -> Builder
    go (ContentRaw s) = fromText $ TS.pack s
    go (ContentVar d) =
        case lookup d cd of
            Just (EPlain s) -> s
            _ -> nothingError "EPlain" d
    go (ContentUrl d) =
        case lookup d cd of
            Just (EUrl u) -> fromText $ render' u []
            _ -> nothingError "EUrl" d
    go (ContentUrlParam d) =
        case lookup d cd of
            Just (EUrlParam (u, p)) ->
                fromText $ render' u p
            _ -> nothingError "EUrlParam" d
    go (ContentMix d) =
        case lookup d cd of
            Just (EMixin m) -> m render'
            _ -> nothingError "EMixin" d
