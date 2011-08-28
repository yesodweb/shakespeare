{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module Text.Hamlet.XML
    ( xml
    , xmlFile
    ) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import qualified Data.Text.Lazy as TL
import Control.Monad ((<=<))
import Text.Hamlet.Parse
import Text.Shakespeare.Base (readUtf8File, derefToExp, Scope, Deref (DerefIdent), Ident (Ident))
import Data.Text (pack, unpack)
import qualified Data.Text as T
import qualified Text.XML.Enumerator.Resolved as X
import Data.String (fromString)
import qualified Data.Foldable as F
import Data.Maybe (fromMaybe)

xml :: QuasiQuoter
xml = QuasiQuoter { quoteExp = strToExp }

xmlFile :: FilePath -> Q Exp
xmlFile = strToExp . TL.unpack <=< qRunIO . readUtf8File

strToExp :: String -> Q Exp
strToExp s =
    case parseDoc s of
        Error e -> error e
        Ok x -> docsToExp [] x

docsToExp :: Scope -> [Doc] -> Q Exp
docsToExp scope docs = [| concat $(fmap ListE $ mapM (docToExp scope) docs) |]

docToExp :: Scope -> Doc -> Q Exp
docToExp scope (DocTag name attrs cs) =
    [| [ X.NodeElement (X.Element ($(liftName name)) $(mkAttrs scope attrs) $(docsToExp scope cs))
       ] |]
docToExp _ (DocContent (ContentRaw s)) = [| [ X.NodeContent (pack $(lift s)) ] |]
docToExp scope (DocContent (ContentVar d)) = [| [ X.NodeContent $(return $ derefToExp scope d) ] |]
docToExp scope (DocContent (ContentEmbed d)) = return $ derefToExp scope d
docToExp scope (DocForall deref ident@(Ident ident') inside) = do
    let list' = derefToExp scope deref
    name <- newName ident'
    let scope' = (ident, VarE name) : scope
    inside' <- docsToExp scope' inside
    let lam = LamE [VarP name] inside'
    [| F.concatMap $(return lam) $(return list') |]
docToExp scope (DocWith [] inside) = docsToExp scope inside
docToExp scope (DocWith ((deref, ident@(Ident name)):dis) inside) = do
    let deref' = derefToExp scope deref
    name' <- newName name
    let scope' = (ident, VarE name') : scope
    inside' <- docToExp scope' (DocWith dis inside)
    let lam = LamE [VarP name'] inside'
    return $ lam `AppE` deref'
docToExp scope (DocMaybe deref ident@(Ident name) just nothing) = do
    let deref' = derefToExp scope deref
    name' <- newName name
    let scope' = (ident, VarE name') : scope
    inside' <- docsToExp scope' just
    let inside'' = LamE [VarP name'] inside'
    nothing' <-
        case nothing of
            Nothing -> [| [] |]
            Just n -> docsToExp scope n
    [| maybe $(return nothing') $(return inside'') $(return deref') |]
docToExp scope (DocCond conds final) = do
    unit <- [| () |]
    body <- fmap GuardedB $ mapM go $ conds ++ [(DerefIdent $ Ident "otherwise", fromMaybe [] final)]
    return $ CaseE unit [Match (TupP []) body []]
  where
    go (deref, inside) = do
        inside' <- docsToExp scope inside
        return (NormalG $ derefToExp scope deref, inside')

mkAttrs :: Scope -> [(Maybe Deref, String, [Content])] -> Q Exp
mkAttrs _ [] = [| [] |]
mkAttrs scope ((mderef, name, value):rest) = do
    rest' <- mkAttrs scope rest
    this <- [| ($(liftName name), T.concat $(fmap ListE $ mapM go value)) |]
    let with = [| $(return this) : $(return rest') |]
    case mderef of
        Nothing -> with
        Just deref -> [| if $(return $ derefToExp scope deref) then $(with) else $(return rest') |]
  where
    go (ContentRaw s) = [| pack $(lift s) |]
    go (ContentVar d) = return $ derefToExp scope d
    go ContentEmbed{} = error "Cannot use embed interpolation in attribute value"

liftName :: String -> Q Exp
liftName s = do
    X.Name local mns _ <- return $ fromString s
    case mns of
        Nothing -> [| X.Name (pack $(lift $ unpack local)) Nothing Nothing |]
        Just ns -> [| X.Name (pack $(lift $ unpack local)) (Just $ pack $(lift $ unpack ns)) Nothing |]
