{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Text.Hamlet.Quasi
    ( hamlet
    , xhamlet
    , hamlet'
    , xhamlet'
    , hamletDebug
    , hamletWithSettings
    , hamletWithSettings'
    , hamletFile
    , xhamletFile
    , hamletFileWithSettings
    , ToHtml (..)
    , varName
    ) where

import Text.Hamlet.Parse
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Char (isUpper)
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Char8 as S8
import Data.Monoid (mconcat, mappend, mempty)
import Text.Blaze (unsafeByteString, Html, string)
import Data.List (intercalate)

class ToHtml a where
    toHtml :: a -> Html ()
instance ToHtml String where
    toHtml = string
instance ToHtml (Html a) where
    toHtml x = x >> return ()

type Scope = [(Ident, Exp)]

docsToExp :: Exp -> Scope -> [Doc] -> Q Exp
docsToExp render scope docs = do
    exps <- mapM (docToExp render scope) docs
    me <- [|mempty|]
    return $
        case exps of
            [] -> me
            [x] -> x
            _ ->
                let x = init exps
                    y = last exps
                    x' = map (BindS WildP) x
                    y' = NoBindS y
                 in DoE $ x' ++ [y']

docToExp :: Exp -> Scope -> Doc -> Q Exp
docToExp render scope (DocForall list ident@(Ident name) inside) = do
    let list' = deref scope list
    name' <- newName name
    let scope' = (ident, VarE name') : scope
    mh <- [|\a -> mconcat . map a|]
    inside' <- docsToExp render scope' inside
    let lam = LamE [VarP name'] inside'
    return $ mh `AppE` lam `AppE` list'
docToExp render scope (DocMaybe val ident@(Ident name) inside mno) = do
    let val' = deref scope val
    name' <- newName name
    let scope' = (ident, VarE name') : scope
    inside' <- docsToExp render scope' inside
    let inside'' = LamE [VarP name'] inside'
    ninside' <- case mno of
                    Nothing -> [|Nothing|]
                    Just no -> do
                        no' <- docsToExp render scope no
                        j <- [|Just|]
                        return $ j `AppE` no'
    mh <- [|maybeH|]
    return $ mh `AppE` val' `AppE` inside'' `AppE` ninside'
docToExp render scope (DocCond conds final) = do
    conds' <- mapM go conds
    final' <- case final of
                Nothing -> [|Nothing|]
                Just f -> do
                    f' <- docsToExp render scope f
                    j <- [|Just|]
                    return $ j `AppE` f'
    ch <- [|condH|]
    return $ ch `AppE` ListE conds' `AppE` final'
  where
    go :: (Deref, [Doc]) -> Q Exp
    go (d, docs) = do
        let d' = deref scope d
        docs' <- docsToExp render scope docs
        return $ TupE [d', docs']
docToExp render v (DocContent c) = contentToExp render v c

contentToExp :: Exp -> Scope -> Content -> Q Exp
contentToExp _ _ (ContentRaw s) = do
    os <- [|unsafeByteString . S8.pack|]
    let s' = LitE $ StringL $ S8.unpack $ BSU.fromString s
    return $ os `AppE` s'
contentToExp _ scope (ContentVar d) = do
    str <- [|toHtml|]
    return $ str `AppE` deref scope d
contentToExp render scope (ContentUrl hasParams d) = do
    ou <- if hasParams
            then [|\r (u, p) -> string $ r u p|]
            else [|\r u -> string $ r u []|]
    let d' = deref scope d
    return $ ou `AppE` render `AppE` d'
contentToExp render scope (ContentEmbed d) = do
    let d' = deref scope d
    return (d' `AppE` render)

-- | Calls 'hamletWithSettings'' with 'defaultHamletSettings'.
hamlet' :: QuasiQuoter
hamlet' = hamletWithSettings' defaultHamletSettings

-- | Calls 'hamletWithSettings'' using XHTML 1.0 Strict settings.
xhamlet' :: QuasiQuoter
xhamlet' = hamletWithSettings' xhtmlHamletSettings

-- | Calls 'hamletWithSettings' with 'defaultHamletSettings'.
hamlet :: QuasiQuoter
hamlet = hamletWithSettings defaultHamletSettings

-- | Calls 'hamletWithSettings' with 'debugHamletSettings'.
hamletDebug :: QuasiQuoter
hamletDebug = hamletWithSettings debugHamletSettings

-- | Calls 'hamletWithSettings' using XHTML 1.0 Strict settings.
xhamlet :: QuasiQuoter
xhamlet = hamletWithSettings xhtmlHamletSettings

-- | A quasi-quoter that converts Hamlet syntax into a function of form:
--
-- > (url -> String) -> Html
--
-- Please see accompanying documentation for a description of Hamlet syntax.
hamletWithSettings :: HamletSettings -> QuasiQuoter
hamletWithSettings set =
    QuasiQuoter (hamletFromString set)
      $ error "Cannot quasi-quote Hamlet to patterns"

-- | A quasi-quoter that converts Hamlet syntax into a 'Html' ().
--
-- Please see accompanying documentation for a description of Hamlet syntax.
hamletWithSettings' :: HamletSettings -> QuasiQuoter
hamletWithSettings' set =
    QuasiQuoter (\s -> do
        x <- hamletFromString set s
        id' <- [|id|]
        return $ x `AppE` id')
    $ error "Cannot quasi-quote Hamlet to patterns"

hamletFromString :: HamletSettings -> String -> Q Exp
hamletFromString set s = do
  case parseDoc set s of
    Error s' -> error s'
    Ok d -> do
        render <- newName "_render"
        func <- docsToExp (VarE render) [] d
        return $ LamE [VarP render] func

hamletFileWithSettings :: HamletSettings -> FilePath -> Q Exp
hamletFileWithSettings set fp = do
    contents <- fmap BSU.toString $ qRunIO $ S8.readFile fp
    hamletFromString set contents

-- | Calls 'hamletFileWithSettings' with 'defaultHamletSettings'.
hamletFile :: FilePath -> Q Exp
hamletFile = hamletFileWithSettings defaultHamletSettings

-- | Calls 'hamletFileWithSettings' using XHTML 1.0 Strict settings.
xhamletFile :: FilePath -> Q Exp
xhamletFile = hamletFileWithSettings xhtmlHamletSettings

deref :: Scope -> Deref -> Exp
deref scope (DerefBranch x y) =
    let x' = deref scope x
        y' = deref scope y
     in x' `AppE` y'
deref scope (DerefLeaf d@(Ident dName)) =
    case lookup d scope of
        Nothing -> varName scope dName
        Just exp' -> exp'

varName :: Scope -> String -> Exp
varName _ "" = error "Illegal empty varName"
varName scope v@(s:_) =
    case lookup (Ident v) scope of
        Just e -> e
        Nothing ->
            if isUpper s
                then ConE $ mkName v
                else VarE $ mkName v

-- | Checks for truth in the left value in each pair in the first argument. If
-- a true exists, then the corresponding right action is performed. Only the
-- first is performed. In there are no true values, then the second argument is
-- performed, if supplied.
condH :: [(Bool, Html ())] -> Maybe (Html ()) -> Html ()
condH [] Nothing = mempty
condH [] (Just x) = x
condH ((True, y):_) _ = y
condH ((False, _):rest) z = condH rest z

-- | Runs the second argument with the value in the first, if available.
-- Otherwise, runs the third argument, if available.
maybeH :: Maybe v -> (v -> Html ()) -> Maybe (Html ()) -> Html ()
maybeH Nothing _ Nothing = mempty
maybeH Nothing _ (Just x) = x
maybeH (Just v) f _ = f v
