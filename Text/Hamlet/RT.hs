{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Text.Hamlet.RT
    ( HamletRT
    , HamletData (..)
    , HamletException (..)
    , parseHamletRT
    , renderHamletRT
    ) where

import Data.Monoid (mconcat)
import Control.Monad (liftM, forM)
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Control.Failure
import Text.Blaze
import Text.Hamlet.Parse
import Data.List (intercalate)

data HamletData = HDHtml (Html ())
                | HDTemplate HamletRT
                | HDMaybe (Maybe HamletData)
                | HDList [HamletData]
                | HDMap [(String, HamletData)]

data SimpleDoc = SDRaw String
               | SDVar [String]
               | SDTemplate [String]
               | SDForall [String] String [SimpleDoc]
               | SDMaybe [String] String [SimpleDoc] [SimpleDoc]

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
    convert x@(DocContent (ContentEmbed deref)) = do
        y <- flattenDeref x deref
        return $ SDTemplate y
    convert x = failure $ HamletUnsupportedDocException x
    flattenDeref _ (DerefLeaf (Ident x)) = return [x]
    flattenDeref orig (DerefBranch (DerefLeaf (Ident x)) y) = do
        y' <- flattenDeref orig y
        return $ x : y'
    flattenDeref orig _ = failure $ HamletUnsupportedDocException orig

renderHamletRT :: Failure HamletException m
               => HamletRT
               -> HamletData
               -> m (Html ())
renderHamletRT (HamletRT docs) (HDMap scope0) =
    liftM mconcat $ mapM (go scope0) docs
  where
    go _ (SDRaw s) = return $ preEscapedString s
    go scope (SDVar n) = do
        v <- lookup' n n $ HDMap scope
        case v of
            HDHtml h -> return h
            _ -> fa $ intercalate "." n ++ ": expected HDHtml"
    go scope (SDTemplate n) = do
        v <- lookup' n n $ HDMap scope
        case v of
            HDTemplate h -> renderHamletRT h $ HDMap scope
            _ -> fa $ intercalate "." n ++ ": expected HDTemplate"
    go scope (SDForall n ident docs') = do
        v <- lookup' n n $ HDMap scope
        case v of
            HDList os -> do
                liftM mconcat $ forM os $ \o -> do
                    let scope' = HDMap $ (ident, o) : scope
                    renderHamletRT (HamletRT docs') scope'
            _ -> fa $ intercalate "." n ++ ": expected HDList"
    go scope (SDMaybe n ident jdocs ndocs) = do
        v <- lookup' n n $ HDMap scope
        (scope', docs') <-
            case v of
                HDMaybe Nothing -> return (scope, ndocs)
                HDMaybe (Just o) -> return ((ident, o) : scope, jdocs)
                _ -> fa $ intercalate "." n ++ ": expected HDMaybe"
        renderHamletRT (HamletRT docs') $ HDMap scope'
    lookup' _ [] x = return x
    lookup' orig (n:ns) (HDMap m) =
        case lookup n m of
            Nothing -> fa $ intercalate "." orig ++ " not found"
            Just o -> lookup' orig ns o
    lookup' orig _ _ = fa $ intercalate "." orig ++ ": unexpected type"
    fa = failure . HamletRenderException
renderHamletRT _ _ =
    failure $ HamletRenderException "renderHamletRT must be given a HDMap"
