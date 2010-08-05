{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Text.Hamlet.RT
    ( HamletRT
    , HamletException (..)
    , parseHamletRT
    , renderHamletRT
    ) where

import Data.Monoid (mconcat)
import Control.Monad (liftM)
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Control.Failure
import Text.Blaze
import Text.Hamlet.Parse
import Data.Object
import Data.List (intercalate)

data SimpleDoc = SDRaw String
               | SDVar [String]

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
        Error s -> failure $ HamletParseException s
        Ok x -> liftM HamletRT $ mapM convert x
  where
    convert (DocContent (ContentRaw s)) = return $ SDRaw s
    convert x@(DocContent (ContentVar deref)) =
        case flattenDeref x deref of
            Nothing -> failure $ HamletUnsupportedDocException x
            Just s -> return $ SDVar s
    convert x = failure $ HamletUnsupportedDocException x
    flattenDeref _ (DerefLeaf (Ident x)) = return [x]
    flattenDeref orig (DerefBranch (DerefLeaf (Ident x)) y) = do
        y' <- flattenDeref orig y
        return $ x : y'
    flattenDeref orig _ = failure $ HamletUnsupportedDocException orig

renderHamletRT :: Failure HamletException m
               => HamletRT
               -> Object String (Html ())
               -> m (Html ())
renderHamletRT (HamletRT docs) scope0 =
    liftM mconcat $ mapM (go scope0) docs
  where
    go _ (SDRaw s) = return $ preEscapedString s
    go scope (SDVar n) = do
        v <- lookup' n n scope
        case v of
            Scalar h -> return h
            x -> fa $ intercalate "." n ++ ": expected scalar"
    lookup' _ [] x = return x
    lookup' orig (n:ns) (Mapping m) =
        case lookup n m of
            Nothing -> fa $ intercalate "." orig ++ " not found"
            Just o -> lookup' orig ns o
    lookup' orig _ _ = fa $ intercalate "." orig ++ ": unexpected type"
    fa = failure. HamletRenderException
