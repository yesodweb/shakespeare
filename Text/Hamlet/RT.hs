{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Provides functionality for runtime Hamlet templates. Please use
-- "Text.Hamlet.Runtime" instead.
module Text.Hamlet.RT
    ( -- * Public API
      HamletRT (..)
    , HamletData (..)
    , HamletMap
    , HamletException (..)
    , parseHamletRT
    , renderHamletRT
    , renderHamletRT'
    , SimpleDoc (..)
    ) where

import Text.Shakespeare.Base
import Data.Monoid (mconcat)
import Control.Monad (liftM, forM)
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Text.Hamlet.Parse
import Data.List (intercalate)
#if MIN_VERSION_blaze_html(0,5,0)
import Text.Blaze.Html (Html)
import Text.Blaze.Internal (preEscapedString, preEscapedText)
#else
import Text.Blaze (preEscapedString, preEscapedText, Html)
#endif
import Data.Text (Text)

#if MIN_VERSION_exceptions(0,4,0)
import Control.Monad.Catch (MonadThrow, throwM)
#else
import Control.Monad.Catch (MonadCatch, throwM)
#define MonadThrow MonadCatch
#endif

type HamletMap url = [([String], HamletData url)]
type UrlRenderer url = (url -> [(Text, Text)] -> Text)

data HamletData url
    = HDHtml Html
    | HDUrl url
    | HDUrlParams url [(Text, Text)]
    | HDTemplate HamletRT
    | HDBool Bool
    | HDMaybe (Maybe (HamletMap url))
    | HDList [HamletMap url]

-- FIXME switch to Text?
data SimpleDoc = SDRaw String
               | SDVar [String]
               | SDUrl Bool [String]
               | SDTemplate [String]
               | SDForall [String] String [SimpleDoc]
               | SDMaybe [String] String [SimpleDoc] [SimpleDoc]
               | SDCond [([String], [SimpleDoc])] [SimpleDoc]

newtype HamletRT = HamletRT [SimpleDoc]

data HamletException = HamletParseException String
                     | HamletUnsupportedDocException Doc
                     | HamletRenderException String
    deriving (Show, Typeable)
instance Exception HamletException



parseHamletRT :: MonadThrow m
              => HamletSettings -> String -> m HamletRT
parseHamletRT set s =
    case parseDoc set s of
        Error s' -> throwM $ HamletParseException s'
        Ok (_, x) -> liftM HamletRT $ mapM convert x
  where
    convert x@(DocForall deref (BindAs _ _) docs) =
       error "Runtime Hamlet does not currently support 'as' patterns"
    convert x@(DocForall deref (BindVar (Ident ident)) docs) = do
        deref' <- flattenDeref' x deref
        docs' <- mapM convert docs
        return $ SDForall deref' ident docs'
    convert DocForall{} = error "Runtime Hamlet does not currently support tuple patterns"
    convert x@(DocMaybe deref (BindAs _ _) jdocs ndocs) =
       error "Runtime Hamlet does not currently support 'as' patterns"
    convert x@(DocMaybe deref (BindVar (Ident ident)) jdocs ndocs) = do
        deref' <- flattenDeref' x deref
        jdocs' <- mapM convert jdocs
        ndocs' <- maybe (return []) (mapM convert) ndocs
        return $ SDMaybe deref' ident jdocs' ndocs'
    convert DocMaybe{} = error "Runtime Hamlet does not currently support tuple patterns"
    convert (DocContent (ContentRaw s')) = return $ SDRaw s'
    convert x@(DocContent (ContentVar deref)) = do
        y <- flattenDeref' x deref
        return $ SDVar y
    convert x@(DocContent (ContentUrl p deref)) = do
        y <- flattenDeref' x deref
        return $ SDUrl p y
    convert x@(DocContent (ContentEmbed deref)) = do
        y <- flattenDeref' x deref
        return $ SDTemplate y
    convert (DocContent ContentMsg{}) =
        error "Runtime hamlet does not currently support message interpolation"
    convert (DocContent ContentAttrs{}) =
        error "Runtime hamlet does not currently support attrs interpolation"

    convert x@(DocCond conds els) = do
        conds' <- mapM go conds
        els' <- maybe (return []) (mapM convert) els
        return $ SDCond conds' els'
      where
        -- | See the comments in Text.Hamlet.Parse.testIncludeClazzes. The conditional
        -- added there doesn't work for runtime Hamlet, so we remove it here.
        go (DerefBranch (DerefIdent x) _, docs') | x == specialOrIdent = do
            docs'' <- mapM convert docs'
            return (["True"], docs'')
        go (deref, docs') = do
            deref' <- flattenDeref' x deref
            docs'' <- mapM convert docs'
            return (deref', docs'')
    convert DocWith{} = error "Runtime hamlet does not currently support $with"
    convert DocCase{} = error "Runtime hamlet does not currently support $case"

renderHamletRT :: MonadThrow m
               => HamletRT
               -> HamletMap url
               -> UrlRenderer url
               -> m Html
renderHamletRT = renderHamletRT' False

#if MIN_VERSION_exceptions(0,4,0)
renderHamletRT' :: MonadThrow m
#else
renderHamletRT' :: MonadCatch m
#endif
                => Bool -- ^ should embeded template (via ^{..}) be plain Html or actual templates?
                -> HamletRT
                -> HamletMap url
                -> (url -> [(Text, Text)] -> Text)
                -> m Html
renderHamletRT' tempAsHtml (HamletRT docs) scope0 renderUrl =
    liftM mconcat $ mapM (go scope0) docs
  where
    go _ (SDRaw s) = return $ preEscapedString s
    go scope (SDVar n) = do
        v <- lookup' n n scope
        case v of
            HDHtml h -> return h
            _ -> fa $ showName n ++ ": expected HDHtml"
    go scope (SDUrl p n) = do
        v <- lookup' n n scope
        case (p, v) of
            (False, HDUrl u) -> return $ preEscapedText $ renderUrl u []
            (True, HDUrlParams u q) ->
                return $ preEscapedText $ renderUrl u q
            (False, _) -> fa $ showName n ++ ": expected HDUrl"
            (True, _) -> fa $ showName n ++ ": expected HDUrlParams"
    go scope (SDTemplate n) = do
        v <- lookup' n n scope
        case (tempAsHtml, v) of
            (False, HDTemplate h) -> renderHamletRT' tempAsHtml h scope renderUrl
            (False, _) -> fa $ showName n ++ ": expected HDTemplate"
            (True, HDHtml h) -> return h
            (True, _) -> fa $ showName n ++ ": expected HDHtml"
    go scope (SDForall n ident docs') = do
        v <- lookup' n n scope
        case v of
            HDList os ->
                liftM mconcat $ forM os $ \o -> do
                    let scope' = map (\(x, y) -> (ident : x, y)) o ++ scope
                    renderHamletRT' tempAsHtml (HamletRT docs') scope' renderUrl
            _ -> fa $ showName n ++ ": expected HDList"
    go scope (SDMaybe n ident jdocs ndocs) = do
        v <- lookup' n n scope
        (scope', docs') <-
            case v of
                HDMaybe Nothing -> return (scope, ndocs)
                HDMaybe (Just o) -> do
                    let scope' = map (\(x, y) -> (ident : x, y)) o ++ scope
                    return (scope', jdocs)
                _ -> fa $ showName n ++ ": expected HDMaybe"
        renderHamletRT' tempAsHtml (HamletRT docs') scope' renderUrl
    go scope (SDCond [] docs') =
        renderHamletRT' tempAsHtml (HamletRT docs') scope renderUrl
    go scope (SDCond ((b, docs'):cs) els) = do
        v <- lookup' b b scope
        case v of
            HDBool True ->
                renderHamletRT' tempAsHtml (HamletRT docs') scope renderUrl
            HDBool False -> go scope (SDCond cs els)
            _ -> fa $ showName b ++ ": expected HDBool"
#if MIN_VERSION_exceptions(0,4,0)
    lookup' :: MonadThrow m
#else
    lookup' :: MonadCatch m
#endif
            => [String] -> [String] -> HamletMap url -> m (HamletData url)
    lookup' orig k m =
        case lookup k m of
            Nothing | k == ["True"] -> return $ HDBool True
            Nothing -> fa $ showName orig ++ ": not found"
            Just x -> return x

fa :: MonadThrow m => String -> m a
fa = throwM . HamletRenderException

showName :: [String] -> String
showName = intercalate "." . reverse

#if MIN_VERSION_exceptions(0,4,0)
flattenDeref' :: MonadThrow f => Doc -> Deref -> f [String]
#else
flattenDeref' :: MonadCatch f => Doc -> Deref -> f [String]
#endif
flattenDeref' orig deref =
    case flattenDeref deref of
        Nothing -> throwM $ HamletUnsupportedDocException orig
        Just x -> return x
