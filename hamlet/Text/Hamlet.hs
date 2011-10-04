{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
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
    , hamlet
    , hamletFile
    , xhamlet
    , xhamletFile
      -- * I18N Hamlet
    , HtmlUrlI18n
    , ihamlet
    , ihamletFile
      -- * Internal, for making more
    , HamletSettings
    , hamletWithSettings
    , hamletFileWithSettings
    , defaultHamletSettings
    , xhtmlHamletSettings
    , Env (..)
    , HamletRules (..)
    ) where

import Text.Shakespeare.Base
import Text.Hamlet.Parse
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Char (isUpper, isDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text.Lazy as TL
import Text.Blaze (Html, preEscapedText, toHtml)
import qualified Data.Foldable as F

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
        _ -> return $ DoE $ map NoBindS exps

docToExp :: Env -> HamletRules -> Scope -> Doc -> Q Exp
docToExp env hr scope (DocForall list ident@(Ident name) inside) = do
    let list' = derefToExp scope list
    name' <- newName name
    let scope' = (ident, VarE name') : scope
    mh <- [|F.mapM_|]
    inside' <- docsToExp env hr scope' inside
    let lam = LamE [VarP name'] inside'
    return $ mh `AppE` lam `AppE` list'
docToExp env hr scope (DocWith [] inside) = do
    inside' <- docsToExp env hr scope inside
    return $ inside'
docToExp env hr scope (DocWith ((deref,ident@(Ident name)):dis) inside) = do
    let deref' = derefToExp scope deref
    name' <- newName name
    let scope' = (ident, VarE name') : scope
    inside' <- docToExp env hr scope' (DocWith dis inside)
    let lam = LamE [VarP name'] inside'
    return $ lam `AppE` deref'
docToExp env hr scope (DocMaybe val ident@(Ident name) inside mno) = do
    let val' = derefToExp scope val
    name' <- newName name
    let scope' = (ident, VarE name') : scope
    inside' <- docsToExp env hr scope' inside
    let inside'' = LamE [VarP name'] inside'
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
        let d' = derefToExp scope d
        docs' <- docsToExp env hr scope docs
        return $ TupE [d', docs']
docToExp env hr v (DocContent c) = contentToExp env hr v c

contentToExp :: Env -> HamletRules -> Scope -> Content -> Q Exp
contentToExp _ hr _ (ContentRaw s) = do
    os <- [|preEscapedText . pack|]
    let s' = LitE $ StringL s
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

shamlet :: QuasiQuoter
shamlet = hamletWithSettings htmlRules defaultHamletSettings

xshamlet :: QuasiQuoter
xshamlet = hamletWithSettings htmlRules xhtmlHamletSettings

htmlRules :: Q HamletRules
htmlRules = do
    i <- [|id|]
    return $ HamletRules i ($ (Env Nothing Nothing)) (\_ b -> return b)

hamlet :: QuasiQuoter
hamlet = hamletWithSettings hamletRules defaultHamletSettings

xhamlet :: QuasiQuoter
xhamlet = hamletWithSettings hamletRules xhtmlHamletSettings

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
    em (Env (Just urender) Nothing) e =
            urender $ \ur' -> return (e `AppE` ur')
    em _ _ = error "bad Env"

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
    case parseDoc set s of
        Error s' -> error s'
        Ok d -> hrWithEnv hr $ \env -> docsToExp env hr [] d

hamletFileWithSettings :: Q HamletRules -> HamletSettings -> FilePath -> Q Exp
hamletFileWithSettings qhr set fp = do
    contents <- fmap TL.unpack $ qRunIO $ readUtf8File fp
    hamletFromString qhr set contents

hamletFile :: FilePath -> Q Exp
hamletFile = hamletFileWithSettings hamletRules defaultHamletSettings

xhamletFile :: FilePath -> Q Exp
xhamletFile = hamletFileWithSettings hamletRules xhtmlHamletSettings

shamletFile :: FilePath -> Q Exp
shamletFile = hamletFileWithSettings htmlRules defaultHamletSettings

xshamletFile :: FilePath -> Q Exp
xshamletFile = hamletFileWithSettings htmlRules xhtmlHamletSettings

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
condH [] Nothing = return ()
condH [] (Just x) = x
condH ((True, y):_) _ = y
condH ((False, _):rest) z = condH rest z

-- | Runs the second argument with the value in the first, if available.
-- Otherwise, runs the third argument, if available.
maybeH :: Monad m => Maybe v -> (v -> m ()) -> Maybe (m ()) -> m ()
maybeH Nothing _ Nothing = return ()
maybeH Nothing _ (Just x) = x
maybeH (Just v) f _ = f v
