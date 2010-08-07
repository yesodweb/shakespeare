{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | Most everything exported here is exported also by "Text.Hamlet". The
-- exceptions to that rule should not be necessary for normal usage.
module Text.Hamlet.RT
    ( -- * Public API
      HamletRT (..)
    , HamletData (..)
    , HamletException (..)
    , parseHamletRT
    , renderHamletRT
    , renderHamletRT'
    , SimpleDoc (..)
    ) where

import Data.Monoid (mconcat)
import Control.Monad (liftM, forM)
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Control.Failure
import Text.Blaze
import Text.Hamlet.Parse
import Text.Hamlet.Quasi (showParams)
import Data.List (intercalate)

data HamletData url = HDHtml (Html ())
                    | HDUrl url
                    | HDUrlParams url [(String, String)]
                    | HDTemplate HamletRT
                    | HDBool Bool
                    | HDMaybe (Maybe (HamletData url))
                    | HDList [HamletData url]
                    | HDMap [(String, HamletData url)]

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
    convert x@(DocForall deref (Ident ident) docs) = do
        deref' <- flattenDeref x deref
        docs' <- mapM convert docs
        return $ SDForall deref' ident docs'
    convert x@(DocMaybe deref (Ident ident) jdocs ndocs) = do
        deref' <- flattenDeref x deref
        jdocs' <- mapM convert jdocs
        ndocs' <- maybe (return []) (mapM convert) ndocs
        return $ SDMaybe deref' ident jdocs' ndocs'
    convert (DocContent (ContentRaw s')) = return $ SDRaw s'
    convert x@(DocContent (ContentVar deref)) = do
        y <- flattenDeref x deref
        return $ SDVar y
    convert x@(DocContent (ContentUrl p deref)) = do
        y <- flattenDeref x deref
        return $ SDUrl p y
    convert x@(DocContent (ContentEmbed deref)) = do
        y <- flattenDeref x deref
        return $ SDTemplate y
    convert x@(DocCond conds els) = do
        conds' <- mapM go conds
        els' <- maybe (return []) (mapM convert) els
        return $ SDCond conds' els'
      where
        go (deref, docs') = do
            deref' <- flattenDeref x deref
            docs'' <- mapM convert docs'
            return (deref', docs'')
    flattenDeref _ (DerefLeaf (Ident x)) = return [x]
    flattenDeref orig (DerefBranch (DerefLeaf (Ident x)) y) = do
        y' <- flattenDeref orig y
        return $ x : y'
    flattenDeref orig _ = failure $ HamletUnsupportedDocException orig

renderHamletRT :: Failure HamletException m
               => HamletRT
               -> HamletData url
               -> (url -> String)
               -> m (Html ())
renderHamletRT = renderHamletRT' False

renderHamletRT' :: Failure HamletException m
                => Bool
                -> HamletRT
                -> HamletData url
                -> (url -> String)
                -> m (Html ())
renderHamletRT' tempAsHtml (HamletRT docs) (HDMap scope0) renderUrl =
    liftM mconcat $ mapM (go scope0) docs
  where
    go _ (SDRaw s) = return $ preEscapedString s
    go scope (SDVar n) = do
        v <- lookup' n n $ HDMap scope
        case v of
            HDHtml h -> return h
            _ -> fa $ intercalate "." n ++ ": expected HDHtml"
    go scope (SDUrl p n) = do
        v <- lookup' n n $ HDMap scope
        case (p, v) of
            (False, HDUrl u) -> return $ preEscapedString $ renderUrl u
            (True, HDUrlParams u q) ->
                return $ preEscapedString $ renderUrl u ++ showParams q
            (False, _) -> fa $ intercalate "." n ++ ": expected HDUrl"
            (True, _) -> fa $ intercalate "." n ++ ": expected HDUrlParams"
    go scope (SDTemplate n) = do
        v <- lookup' n n $ HDMap scope
        case (tempAsHtml, v) of
            (False, HDTemplate h) -> renderHamletRT h (HDMap scope) renderUrl
            (False, _) -> fa $ intercalate "." n ++ ": expected HDTemplate"
            (True, HDHtml h) -> return h
            (True, _) -> fa $ intercalate "." n ++ ": expected HDHtml"
    go scope (SDForall n ident docs') = do
        v <- lookup' n n $ HDMap scope
        case v of
            HDList os -> do
                liftM mconcat $ forM os $ \o -> do
                    let scope' = HDMap $ (ident, o) : scope
                    renderHamletRT (HamletRT docs') scope' renderUrl
            _ -> fa $ intercalate "." n ++ ": expected HDList"
    go scope (SDMaybe n ident jdocs ndocs) = do
        v <- lookup' n n $ HDMap scope
        (scope', docs') <-
            case v of
                HDMaybe Nothing -> return (scope, ndocs)
                HDMaybe (Just o) -> return ((ident, o) : scope, jdocs)
                _ -> fa $ intercalate "." n ++ ": expected HDMaybe"
        renderHamletRT (HamletRT docs') (HDMap scope') renderUrl
    go scope (SDCond [] docs') =
        renderHamletRT (HamletRT docs') (HDMap scope) renderUrl
    go scope (SDCond ((b, docs'):cs) els) = do
        v <- lookup' b b $ HDMap scope
        case v of
            HDBool True ->
                renderHamletRT (HamletRT docs') (HDMap scope) renderUrl
            HDBool False -> go scope (SDCond cs els)
            _ -> fa $ intercalate "." b ++ ": expected HDBool"
    lookup' _ [] x = return x
    lookup' orig (n:ns) (HDMap m) =
        case lookup n m of
            Nothing -> fa $ intercalate "." orig ++ " not found"
            Just o -> lookup' orig ns o
    lookup' orig _ _ = fa $ intercalate "." orig ++ ": unexpected type"
    fa = failure . HamletRenderException
renderHamletRT' _ _ _ _ =
    failure $ HamletRenderException "renderHamletRT must be given a HDMap"
