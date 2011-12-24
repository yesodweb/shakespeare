{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Most everything exported here is exported also by "Text.Hamlet". The
-- exceptions to that rule should not be necessary for normal usage.
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
import Control.Failure
import Text.Hamlet.Parse
import Data.List (intercalate)
import Text.Blaze (preEscapedString, preEscapedText, Html)
import Data.Text (Text)

type HamletMap url = [([String], HamletData url)]

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

parseHamletRT :: Failure HamletException m
              => HamletSettings -> String -> m HamletRT
parseHamletRT set s =
    case parseDoc set s of
        Error s' -> failure $ HamletParseException s'
        Ok x -> liftM HamletRT $ mapM convert x
  where
    convert x@(DocForall deref [Ident ident] docs) = do
        deref' <- flattenDeref' x deref
        docs' <- mapM convert docs
        return $ SDForall deref' ident docs'
    convert DocForall{} = error "Runtime Hamlet does not currently support tuple patterns"
    convert x@(DocMaybe deref [Ident ident] jdocs ndocs) = do
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
    convert x@(DocCond conds els) = do
        conds' <- mapM go conds
        els' <- maybe (return []) (mapM convert) els
        return $ SDCond conds' els'
      where
        go (deref, docs') = do
            deref' <- flattenDeref' x deref
            docs'' <- mapM convert docs'
            return (deref', docs'')
    convert DocWith{} = error "Runtime hamlet does not currently support $with"
    convert DocSwitch{} = error "Runtime hamlet does not currently support $switch"

renderHamletRT :: Failure HamletException m
               => HamletRT
               -> HamletMap url
               -> (url -> [(Text, Text)] -> Text)
               -> m Html
renderHamletRT = renderHamletRT' False

renderHamletRT' :: Failure HamletException m
                => Bool
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
    lookup' :: Failure HamletException m
            => [String] -> [String] -> HamletMap url -> m (HamletData url)
    lookup' orig k m =
        case lookup k m of
            Nothing -> fa $ showName orig ++ ": not found"
            Just x -> return x

fa :: Failure HamletException m => String -> m a
fa = failure . HamletRenderException

showName :: [String] -> String
showName = intercalate "." . reverse

flattenDeref' :: Failure HamletException f => Doc -> Deref -> f [String]
flattenDeref' orig deref =
    case flattenDeref deref of
        Nothing -> failure $ HamletUnsupportedDocException orig
        Just x -> return x
