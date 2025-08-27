{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Text.Hamlet
    ( -- * Plain HTML
      Html
    , shamlet
    , shamletFile
    , xshamlet
    , xshamletFile
      -- * Hamlet
    , HtmlUrl
    , Render
    , hamlet
    , hamletFile
    , hamletFileReload
    , xhamlet
    , xhamletFile
      -- * I18N Hamlet
    , HtmlUrlI18n
    , Translate
    , ihamlet
    , ihamletFile
    , ihamletFileReload
      -- * Type classes
    , ToAttributes (..)
      -- * Internal, for making more
    , HamletSettings (..)
    , NewlineStyle (..)
    , hamletWithSettings
    , hamletFileWithSettings
    , defaultHamletSettings
    , xhtmlHamletSettings
    , Env (..)
    , HamletRules (..)
    , hamletRules
    , ihamletRules
    , htmlRules
    , CloseStyle (..)
      -- * Used by generated code
    , condH
    , maybeH
    , asHtmlUrl
    , attrsToHtml
     -- * low-level
    , hamletFromString
    ) where

import Text.Blaze.Internal (unsafeByteString)
import Text.Shakespeare.Base
import Text.Hamlet.Parse
import Language.Haskell.TH.Syntax hiding (Module)
import Language.Haskell.TH.Quote
import Data.Char (isUpper, isDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Internal (preEscapedText)
import qualified Data.Foldable as F
import Control.Monad (mplus)
import Data.Monoid (mempty, mappend, mconcat)
import Control.Arrow ((***))
import Data.List (intercalate)

import Data.IORef
import qualified Data.Map as M
import System.IO.Unsafe (unsafePerformIO)
import System.Directory (getModificationTime)
import Data.Time (UTCTime)
import Text.Blaze.Html (preEscapedToHtml)

-- | Convert some value to a list of attribute pairs.
class ToAttributes a where
    toAttributes :: a -> [(Text, Text)]
instance ToAttributes (Text, Text) where
    toAttributes = return
instance ToAttributes (String, String) where
    toAttributes (k, v) = [(pack k, pack v)]
instance ToAttributes [(Text, Text)] where
    toAttributes = id
instance ToAttributes [(String, String)] where
    toAttributes = map (pack *** pack)

attrsToHtml :: [(Text, Text)] -> Html
attrsToHtml =
    foldr go mempty
  where
    go (k, v) rest =
        toHtml " "
        `mappend` preEscapedText k
        `mappend` preEscapedText (pack "=\"")
        `mappend` toHtml v
        `mappend` preEscapedText (pack "\"")
        `mappend` rest

type Render url = url -> [(Text, Text)] -> Text
type Translate msg = msg -> Html

-- | A function generating an 'Html' given a URL-rendering function.
type HtmlUrl url = Render url -> Html

-- | A function generating an 'Html' given a message translator and a URL rendering function.
type HtmlUrlI18n msg url = Translate msg -> Render url -> Html

docsToExp :: Env -> HamletRules -> Scope -> [Doc] -> Q Exp
docsToExp env hr scope docs = do
    exps <- mapM (docToExp env hr scope) docs
    case exps of
        [] -> [|return ()|]
        [x] -> return x
        _ -> return $ DoE
#if MIN_VERSION_template_haskell(2,17,0)
                Nothing
#endif
                $ map NoBindS exps

unIdent :: Ident -> String
unIdent (Ident s) = s

bindingPattern :: Binding -> Q (Pat, [(Ident, Exp)])
bindingPattern (BindAs i@(Ident s) b) = do
    name <- newName s
    (newPattern, scope) <- bindingPattern b
    return (AsP name newPattern, (i, VarE name):scope)
bindingPattern (BindVar i@(Ident s))
    | s == "_" = return (WildP, [])
    | all isDigit s = do
        return (LitP $ IntegerL $ read s, [])
    | otherwise = do
        name <- newName s
        return (VarP name, [(i, VarE name)])
bindingPattern (BindTuple is) = do
    (patterns, scopes) <- fmap unzip $ mapM bindingPattern is
    return (TupP patterns, concat scopes)
bindingPattern (BindList is) = do
    (patterns, scopes) <- fmap unzip $ mapM bindingPattern is
    return (ListP patterns, concat scopes)
bindingPattern (BindConstr con is) = do
    (patterns, scopes) <- fmap unzip $ mapM bindingPattern is
    return (conP (mkConName con) patterns, concat scopes)
bindingPattern (BindRecord con fields wild) = do
    let f (Ident field,b) =
           do (p,s) <- bindingPattern b
              return ((mkName field,p),s)
    (patterns, scopes) <- fmap unzip $ mapM f fields
    (patterns1, scopes1) <- if wild
       then bindWildFields con $ map fst fields
       else return ([],[])
    return (RecP (mkConName con) (patterns++patterns1), concat scopes ++ scopes1)

mkConName :: DataConstr -> Name
mkConName = mkName . conToStr

conToStr :: DataConstr -> String
conToStr (DCUnqualified (Ident x)) = x
conToStr (DCQualified (Module xs) (Ident x)) = intercalate "." $ xs ++ [x]

conP :: Name -> [Pat] -> Pat
#if MIN_VERSION_template_haskell(2,18,0)
conP name = ConP name []
#else
conP = ConP
#endif

-- Wildcards bind all of the unbound fields to variables whose name
-- matches the field name.
--
-- For example: data R = C { f1, f2 :: Int }
-- C {..}           is equivalent to   C {f1=f1, f2=f2}
-- C {f1 = a, ..}   is equivalent to   C {f1=a,  f2=f2}
-- C {f2 = a, ..}   is equivalent to   C {f1=f1, f2=a}
bindWildFields :: DataConstr -> [Ident] -> Q ([(Name, Pat)], [(Ident, Exp)])
bindWildFields conName fields = do
  fieldNames <- recordToFieldNames conName
  let available n     = nameBase n `notElem` map unIdent fields
  let remainingFields = filter available fieldNames
  let mkPat n = do
        e <- newName (nameBase n)
        return ((n,VarP e), (Ident (nameBase n), VarE e))
  fmap unzip $ mapM mkPat remainingFields

-- Important note! reify will fail if the record type is defined in the
-- same module as the reify is used. This means quasi-quoted Hamlet
-- literals will not be able to use wildcards to match record types
-- defined in the same module.
recordToFieldNames :: DataConstr -> Q [Name]
recordToFieldNames conStr = do
  -- use 'lookupValueName' instead of just using 'mkName' so we reify the
  -- data constructor and not the type constructor if their names match.
  Just conName                <- lookupValueName $ conToStr conStr
  DataConI _ _ typeName         <- reify conName
  TyConI (DataD _ _ _ _ cons _) <- reify typeName
  [fields] <- return [fields | RecC name fields <- cons, name == conName]
  return [fieldName | (fieldName, _, _) <- fields]

docToExp :: Env -> HamletRules -> Scope -> Doc -> Q Exp
docToExp env hr scope (DocForall list idents inside) = do
    let list' = derefToExp scope list
    (pat, extraScope) <- bindingPattern idents
    let scope' = extraScope ++ scope
    mh <- [|F.mapM_|]
    inside' <- docsToExp env hr scope' inside
    let lam = LamE [pat] inside'
    return $ mh `AppE` lam `AppE` list'
docToExp env hr scope (DocWith [] inside) = do
    inside' <- docsToExp env hr scope inside
    return $ inside'
docToExp env hr scope (DocWith ((deref, idents):dis) inside) = do
    let deref' = derefToExp scope deref
    (pat, extraScope) <- bindingPattern idents
    let scope' = extraScope ++ scope
    inside' <- docToExp env hr scope' (DocWith dis inside)
    let lam = LamE [pat] inside'
    return $ lam `AppE` deref'
docToExp env hr scope (DocComponent mbinding deref inside) = do
    let deref' = derefToExp scope deref
    case mbinding of
        Just binding -> do
            (pat, extraScope) <- bindingPattern binding
            let scope' = extraScope ++ scope
            inside' <-
                hrWithEnv hr $ \env ->
                    docsToExp env hr scope' inside
            hrEmbed hr env $ deref' `AppE` LamE [pat] inside'
        Nothing -> do
            inside' <-
                hrWithEnv hr $ \env ->
                    docsToExp env hr scope inside
            hrEmbed hr env $ deref' `AppE` inside'

docToExp env hr scope (DocMaybe val idents inside mno) = do
    let val' = derefToExp scope val
    (pat, extraScope) <- bindingPattern idents
    let scope' = extraScope ++ scope
    inside' <- docsToExp env hr scope' inside
    let inside'' = LamE [pat] inside'
    ninside' <- case mno of
                    Nothing -> [|Nothing|]
                    Just no -> do
                        no' <- docsToExp env hr scope no
                        j <- [|Just|]
                        return $ j `AppE` no'
    mh <- [|maybeH|]
    return $ mh `AppE` val' `AppE` inside'' `AppE` ninside'
docToExp env hr scope (DocCond conds final) = do
    conds' <- mapM go conds
    final' <- case final of
                Nothing -> [|Nothing|]
                Just f -> do
                    f' <- docsToExp env hr scope f
                    j <- [|Just|]
                    return $ j `AppE` f'
    ch <- [|condH|]
    return $ ch `AppE` ListE conds' `AppE` final'
  where
    go :: (Deref, [Doc]) -> Q Exp
    go (d, docs) = do
        let d' = derefToExp ((specialOrIdent, VarE 'or):scope) d
        docs' <- docsToExp env hr scope docs
        return $ TupE
#if MIN_VERSION_template_haskell(2,16,0)
          $ map Just
#endif
          [d', docs']
docToExp env hr scope (DocCase deref cases) = do
    let exp_ = derefToExp scope deref
    matches <- mapM toMatch cases
    return $ CaseE exp_ matches
  where
    toMatch :: (Binding, [Doc]) -> Q Match
    toMatch (idents, inside) = do
        (pat, extraScope) <- bindingPattern idents
        let scope' = extraScope ++ scope
        insideExp <- docsToExp env hr scope' inside
        return $ Match pat (NormalB insideExp) []
docToExp env hr v (DocContent c) = contentToExp env hr v c

contentToExp :: Env -> HamletRules -> Scope -> Content -> Q Exp
contentToExp _ hr _ (ContentRaw s) = do
    os <- [|unsafeByteString|]
    s' <- lift $ TE.encodeUtf8 (pack s)
    return $ hrFromHtml hr `AppE` (os `AppE` s')
contentToExp _ hr scope (ContentVar d) = do
    str <- [|toHtml|]
    return $ hrFromHtml hr `AppE` (str `AppE` derefToExp scope d)
contentToExp env hr scope (ContentUrl hasParams d) =
    case urlRender env of
        Nothing -> error "URL interpolation used, but no URL renderer provided"
        Just wrender -> wrender $ \render -> do
            let render' = return render
            ou <- if hasParams
                    then [|\(u, p) -> $(render') u p|]
                    else [|\u -> $(render') u []|]
            let d' = derefToExp scope d
            pet <- [|toHtml|]
            return $ hrFromHtml hr `AppE` (pet `AppE` (ou `AppE` d'))
contentToExp env hr scope (ContentEmbed d) = hrEmbed hr env $ derefToExp scope d
contentToExp env hr scope (ContentMsg d) =
    case msgRender env of
        Nothing -> error "Message interpolation used, but no message renderer provided"
        Just wrender -> wrender $ \render ->
            return $ hrFromHtml hr `AppE` (render `AppE` derefToExp scope d)
contentToExp _ hr scope (ContentAttrs d) = do
    html <- [|attrsToHtml . toAttributes|]
    return $ hrFromHtml hr `AppE` (html `AppE` derefToExp scope d)

-- | "Simple Hamlet" quasi-quoter. May only be used to generate expressions.
--
-- Generated expressions have type 'Html'.
--
-- @
-- >>> 'putStrLn' ('Text.Blaze.Html.Renderer.renderHtml' ['shamlet'|\<div\>Hello, world!|])
-- \<div\>Hello, world!\</div\>
-- @
shamlet :: QuasiQuoter
shamlet = hamletWithSettings htmlRules defaultHamletSettings

-- | Like 'shamlet', but produces XHTML.
xshamlet :: QuasiQuoter
xshamlet = hamletWithSettings htmlRules xhtmlHamletSettings

htmlRules :: Q HamletRules
htmlRules = do
    i <- [|id|]
    return $ HamletRules i ($ (Env Nothing Nothing)) (\_ b -> return b)

-- | Hamlet quasi-quoter. May only be used to generate expressions.
--
-- Generated expression have type @'HtmlUrl' url@, for some @url@.
--
-- @
-- data MyRoute = Home
--
-- render :: 'Render' MyRoute
-- render Home _ = \"/home\"
--
-- >>> 'putStrLn' ('Text.Blaze.Html.Renderer.String.renderHtml' (['hamlet'|\<a href=@{Home}\>Home|] render))
-- \<a href="\/home"\>Home\<\/a\>
-- @
hamlet :: QuasiQuoter
hamlet = hamletWithSettings hamletRules defaultHamletSettings

-- | Like 'hamlet', but produces XHTML.
xhamlet :: QuasiQuoter
xhamlet = hamletWithSettings hamletRules xhtmlHamletSettings

asHtmlUrl :: HtmlUrl url -> HtmlUrl url
asHtmlUrl = id

hamletRules :: Q HamletRules
hamletRules = do
    i <- [|id|]
    let ur f = do
            r <- newName "_render"
            let env = Env
                    { urlRender = Just ($ (VarE r))
                    , msgRender = Nothing
                    }
            h <- f env
            return $ LamE [VarP r] h
    return $ HamletRules i ur em
  where
    em (Env (Just urender) Nothing) e = do
        asHtmlUrl' <- [|asHtmlUrl|]
        urender $ \ur' -> return ((asHtmlUrl' `AppE` e) `AppE` ur')
    em _ _ = error "bad Env"

-- | Hamlet quasi-quoter with internationalization. May only be used to generate
-- expressions.
--
-- Generated expressions have type @'HtmlUrlI18n' msg url@, for some @msg@ and
-- @url@.
--
-- @
-- data MyMsg = Hi | Bye
--
-- data MyRoute = Home
--
-- renderEnglish :: 'Translate' MyMsg
-- renderEnglish Hi  = \"hi\"
-- renderEnglish Bye = \"bye\"
--
-- renderUrl :: 'Render' MyRoute
-- renderUrl Home _ = \"/home\"
--
-- >>> 'putStrLn' ('Text.Blaze.Html.Renderer.renderHtml' (['ihamlet'|@{Home} _{Hi} _{Bye}|] renderEnglish renderUrl))
-- \<div\>/home hi bye \<div\>
-- @
ihamlet :: QuasiQuoter
ihamlet = hamletWithSettings ihamletRules defaultHamletSettings

ihamletRules :: Q HamletRules
ihamletRules = do
    i <- [|id|]
    let ur f = do
            u <- newName "_urender"
            m <- newName "_mrender"
            let env = Env
                    { urlRender = Just ($ (VarE u))
                    , msgRender = Just ($ (VarE m))
                    }
            h <- f env
            return $ LamE [VarP m, VarP u] h
    return $ HamletRules i ur em
  where
    em (Env (Just urender) (Just mrender)) e =
          urender $ \ur' -> mrender $ \mr -> return (e `AppE` mr `AppE` ur')
    em _ _ = error "bad Env"

-- | Quasiquoter that follows XHTML serialization rules and supports i18n.
--
-- @since 2.0.10
ixhamlet :: QuasiQuoter
ixhamlet = hamletWithSettings ihamletRules xhtmlHamletSettings

hamletWithSettings :: Q HamletRules -> HamletSettings -> QuasiQuoter
hamletWithSettings hr set =
    QuasiQuoter
        { quoteExp = hamletFromString hr set
        }

data HamletRules = HamletRules
    { hrFromHtml :: Exp
    , hrWithEnv :: (Env -> Q Exp) -> Q Exp
    , hrEmbed :: Env -> Exp -> Q Exp
    }

data Env = Env
    { urlRender :: Maybe ((Exp -> Q Exp) -> Q Exp)
    , msgRender :: Maybe ((Exp -> Q Exp) -> Q Exp)
    }

hamletFromString :: Q HamletRules -> HamletSettings -> String -> Q Exp
hamletFromString qhr set s = do
    hr <- qhr
    hrWithEnv hr $ \env -> docsToExp env hr [] $ docFromString set s

docFromString :: HamletSettings -> String -> [Doc]
docFromString set s =
    case parseDoc set s of
        Error s' -> error s'
        Ok (_, d) -> d

hamletFileWithSettings :: Q HamletRules -> HamletSettings -> FilePath -> Q Exp
hamletFileWithSettings qhr set fp = do
    contents <- readFileRecompileQ fp
    hamletFromString qhr set contents

-- | Like 'hamlet', but reads an external file at compile time.
--
-- @
-- $('hamletFile' \"foo.hamlet\") :: 'HtmlUrl' MyRoute
-- @
hamletFile :: FilePath -> Q Exp
hamletFile = hamletFileWithSettings hamletRules defaultHamletSettings

-- | Like 'hamletFile', but the external file is parsed at runtime. Allows for
-- more rapid development, but should not be used in production.
hamletFileReload :: FilePath -> Q Exp
hamletFileReload = hamletFileReloadWithSettings runtimeRules defaultHamletSettings
  where runtimeRules = HamletRuntimeRules { hrrI18n = False }

-- | Like 'ihamletFile', but the external file is parsed at runtime. Allows for
-- more rapid development, but should not be used in production.
ihamletFileReload :: FilePath -> Q Exp
ihamletFileReload = hamletFileReloadWithSettings runtimeRules defaultHamletSettings
  where runtimeRules = HamletRuntimeRules { hrrI18n = True }

-- | Like 'hamletFile', but produces XHTML.
xhamletFile :: FilePath -> Q Exp
xhamletFile = hamletFileWithSettings hamletRules xhtmlHamletSettings

-- | Like 'shamlet', but reads an external file at compile time.
--
-- @
-- $('shamletFile' \"foo.hamlet\") :: 'Html'
-- @
shamletFile :: FilePath -> Q Exp
shamletFile = hamletFileWithSettings htmlRules defaultHamletSettings

-- | Like 'shamletFile', but produces XHTML.
xshamletFile :: FilePath -> Q Exp
xshamletFile = hamletFileWithSettings htmlRules xhtmlHamletSettings

-- | Like 'ihamlet', but reads an external file at compile time.
--
-- @
-- $('ihamletFile' \"foo.hamlet\") :: 'HtmlUrlI18n' MyMsg MyRoute
-- @
ihamletFile :: FilePath -> Q Exp
ihamletFile = hamletFileWithSettings ihamletRules defaultHamletSettings

varName :: Scope -> String -> Exp
varName _ "" = error "Illegal empty varName"
varName scope v@(_:_) = fromMaybe (strToExp v) $ lookup (Ident v) scope

strToExp :: String -> Exp
strToExp s@(c:_)
    | all isDigit s = LitE $ IntegerL $ read s
    | isUpper c = ConE $ mkName s
    | otherwise = VarE $ mkName s
strToExp "" = error "strToExp on empty string"

-- | Checks for truth in the left value in each pair in the first argument. If
-- a true exists, then the corresponding right action is performed. Only the
-- first is performed. In there are no true values, then the second argument is
-- performed, if supplied.
condH :: Monad m => [(Bool, m ())] -> Maybe (m ()) -> m ()
condH bms mm = fromMaybe (return ()) $ lookup True bms `mplus` mm

-- | Runs the second argument with the value in the first, if available.
-- Otherwise, runs the third argument, if available.
maybeH :: Monad m => Maybe v -> (v -> m ()) -> Maybe (m ()) -> m ()
maybeH mv f mm = fromMaybe (return ()) $ fmap f mv `mplus` mm


type MTime = UTCTime
data VarType = VTPlain | VTUrl | VTUrlParam | VTMixin | VTMsg | VTAttrs

type QueryParameters = [(Text, Text)]
type RenderUrl url   = (url -> QueryParameters -> Text)
type Shakespeare url = RenderUrl url -> Html
data VarExp msg url  = EPlain Html
                     | EUrl url
                     | EUrlParam (url, QueryParameters)
                     | EMixin (HtmlUrl url)
                     | EMixinI18n (HtmlUrlI18n msg url)
                     | EMsg msg

instance Show (VarExp msg url) where
  show (EPlain html) = "EPlain"
  show (EUrl url) = "EUrl"
  show (EUrlParam url) = "EUrlParam"
  show (EMixin url) = "EMixin"
  show (EMixinI18n msg_url) = "EMixinI18n"
  show (EMsg msg) = "EMsg"

getVars :: Content -> [(Deref, VarType)]
getVars ContentRaw{}     = []
getVars (ContentVar d)   = [(d, VTPlain)]
getVars (ContentUrl False d) = [(d, VTUrl)]
getVars (ContentUrl True d) = [(d, VTUrlParam)]
getVars (ContentEmbed d) = [(d, VTMixin)]
getVars (ContentMsg d)   = [(d, VTMsg)]
getVars (ContentAttrs d) = [(d, VTAttrs)]

hamletUsedIdentifiers :: HamletSettings -> String -> [(Deref, VarType)]
hamletUsedIdentifiers settings =
    concatMap getVars . contentFromString settings


data HamletRuntimeRules = HamletRuntimeRules {
                            hrrI18n :: Bool
                          }

hamletFileReloadWithSettings :: HamletRuntimeRules
                             -> HamletSettings -> FilePath -> Q Exp
hamletFileReloadWithSettings hrr settings fp = do
    s <- readFileQ fp
    let b = hamletUsedIdentifiers settings s
    c <- mapM vtToExp b
    rt <- if hrrI18n hrr
      then [|hamletRuntimeMsg settings fp|]
      else [|hamletRuntime settings fp|]
    return $ rt `AppE` ListE c
  where
    vtToExp :: (Deref, VarType) -> Q Exp
    vtToExp (d, vt) = do
        d' <- lift d
        c' <- toExp vt
        return $ TupE
#if MIN_VERSION_template_haskell(2,16,0)
          $ map Just
#endif
          [d', c' `AppE` derefToExp [] d]
      where
        toExp = c
          where
            c :: VarType -> Q Exp
            c VTAttrs = [|EPlain . attrsToHtml . toAttributes|]
            c VTPlain = [|EPlain . toHtml|]
            c VTUrl = [|EUrl|]
            c VTUrlParam = [|EUrlParam|]
            c VTMixin = [|\r -> EMixin $ \c -> r c|]
            c VTMsg = [|EMsg|]

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

contentFromString :: HamletSettings -> String -> [Content]
contentFromString set = map justContent . docFromString set
  where
    unsupported msg = error $ "hamletFileReload does not support " ++ msg

    justContent :: Doc -> Content
    justContent (DocContent c) = c
    justContent DocForall{} = unsupported "$forall"
    justContent DocWith{} = unsupported "$with"
    justContent DocComponent{} = unsupported "$component"
    justContent DocMaybe{} = unsupported "$maybe"
    justContent DocCase{} = unsupported "$case"
    justContent DocCond{} = unsupported "attribute conditionals"


hamletRuntime :: HamletSettings
              -> FilePath
              -> [(Deref, VarExp msg url)]
              -> Shakespeare url
hamletRuntime settings fp cd render = unsafePerformIO $ do
    mtime <- qRunIO $ getModificationTime fp
    mdata <- lookupReloadMap fp
    case mdata of
      Just (lastMtime, lastContents) ->
        if mtime == lastMtime then return $ go' lastContents
          else fmap go' $ newContent mtime
      Nothing -> fmap go' $ newContent mtime
  where
    newContent mtime = do
        s <- readUtf8FileString fp
        insertReloadMap fp (mtime, contentFromString settings s)

    go' = mconcat . map (runtimeContentToHtml cd render (error "I18n embed IMPOSSIBLE") handleMsgEx)
    handleMsgEx _ = error "i18n _{} encountered, but did not use ihamlet"

type RuntimeVars msg url = [(Deref, VarExp msg url)]
hamletRuntimeMsg :: HamletSettings
              -> FilePath
              -> RuntimeVars msg url
              -> HtmlUrlI18n msg url
hamletRuntimeMsg settings fp cd i18nRender render = unsafePerformIO $ do
    mtime <- qRunIO $ getModificationTime fp
    mdata <- lookupReloadMap fp
    case mdata of
      Just (lastMtime, lastContents) ->
        if mtime == lastMtime then return $ go' lastContents
          else fmap go' $ newContent mtime
      Nothing -> fmap go' $ newContent mtime
  where
    newContent mtime = do
        s <- readUtf8FileString fp
        insertReloadMap fp (mtime, contentFromString settings s)

    go' = mconcat . map (runtimeContentToHtml cd render i18nRender handleMsg)
    handleMsg d = case lookup d cd of
            Just (EMsg s) -> i18nRender s
            _ -> nothingError "EMsg for ContentMsg" d

nothingError :: Show a => String -> a -> b
nothingError expected d = error $ "expected " ++ expected ++ " but got Nothing for: " ++ show d

runtimeContentToHtml :: RuntimeVars msg url -> Render url -> Translate msg -> (Deref -> Html) -> Content -> Html
runtimeContentToHtml cd render i18nRender handleMsg = go
  where
    go :: Content -> Html
    go (ContentMsg d) = handleMsg d
    go (ContentRaw s) = preEscapedToHtml s
    go (ContentAttrs d) =
        case lookup d cd of
            Just (EPlain s) -> s
            _ -> error $ show d ++ ": expected EPlain for ContentAttrs"
    go (ContentVar d) =
        case lookup d cd of
            Just (EPlain s) -> s
            _ -> error $ show d ++ ": expected EPlain for ContentVar"
    go (ContentUrl False d) =
        case lookup d cd of
            Just (EUrl u) -> toHtml $ render u []
            Just wrong -> error $  "expected EUrl but got: " ++ show wrong ++ "\nfor: " ++ show d
            _ -> nothingError "EUrl" d
    go (ContentUrl True d) =
        case lookup d cd of
            Just (EUrlParam (u, p)) ->
                toHtml $ render u p
            _ -> error $ show d ++ ": expected EUrlParam"
    go (ContentEmbed d) = case lookup d cd of
        Just (EMixin m) -> m render
        Just (EMixinI18n m) -> m i18nRender render
        _ -> error $ show d ++ ": expected EMixin"
