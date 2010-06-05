{-# LANGUAGE TemplateHaskell #-}
module Text.Hamlet.Quasi
    ( hamlet
    , xhamlet
    , hamletWithSettings
    ) where

import Text.Hamlet.Parse
import Text.Hamlet.Monad
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Control.Monad
import Data.List (sortBy, isPrefixOf)
import Data.Char (isUpper)
import Control.Applicative ((<$>))
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Char8 as S8

type Vars = (Scope, [Stmt] -> [Stmt])

type Scope = [([Ident], Exp)]

docsToExp :: [Doc] -> Q Exp
docsToExp docs = do
    (_, stmts) <- foldM docToStmt ([], id) docs
    safeDoE $ stmts []

safeDoE :: [Stmt] -> Q Exp
safeDoE [] = [|return ()|]
safeDoE x = return $ DoE x

docToStmt :: Vars -> Doc -> Q Vars
docToStmt (vars, stmts) (DocForall (Deref idents) ident@(Ident name) inside) = do
    (vars', deref', stmts') <-
        case idents of
            [] -> error $ "Invalid forall deref: " ++ show idents
            [(Ident i)] -> do
                return (vars, getBase vars $ Ident i, id)
            _ -> do
                let front = Deref $ init idents
                    Ident i = last idents
                (vars', base, stmts') <- bindDeref vars front
                return (vars', VarE (mkName i) `AppE` base, stmts')
    let deref'' = deref'
    mh <- [|mapH|]
    ident' <- newName name
    let vars'' = ([ident], VarE ident') : vars'
    (_, inside') <- foldM docToStmt (vars'', id) inside
    dos <- LamE [VarP ident'] <$> (safeDoE $ inside' [])
    let stmt = NoBindS $ mh `AppE` dos `AppE` deref''
    return (vars', stmts . stmts' . (:) stmt)
docToStmt (vars, stmts) (DocMaybe deref ident@(Ident name) inside mno) = do
    (vars', base, stmts') <- bindDeref vars deref
    ident' <- newName name
    let vars'' = ([ident], VarE ident') : vars'
    (_, inside') <- foldM docToStmt (vars'', id) inside
    dos <- LamE [VarP ident'] <$> (safeDoE $ inside' [])
    ninside <- case mno of
                Nothing -> [|Nothing|]
                Just no -> do
                    (_, ninside') <- foldM docToStmt (vars'', id) no
                    j <- [|Just|]
                    x <- safeDoE $ ninside' []
                    return $ j `AppE` x
    mh <- [|maybeH|]
    let stmt = NoBindS $ mh `AppE` base `AppE` dos `AppE` ninside
    return (vars', stmts . stmts' . (:) stmt)
docToStmt (vars, stmts) (DocCond conds final) = do
    conds' <- liftConds vars conds id
    final' <- case final of
                Nothing -> [|Nothing|]
                Just f -> do
                    (_, f') <- foldM docToStmt (vars, id) f
                    j <- [|Just|]
                    AppE j <$> (safeDoE $ f' [])
    ch <- [|condH|]
    let stmt = NoBindS $ ch `AppE` conds' `AppE` final'
    return (vars, stmts . (:) stmt)
docToStmt v (DocContent c) = foldM contentToStmt v c

contentToStmt :: Vars -> Content -> Q Vars
contentToStmt (a, b) (ContentRaw s) = do
    os <- [|outputOctets|]
    let s' = LitE $ StringL $ S8.unpack $ BSU.fromString s
    let stmt = NoBindS $ os `AppE` s'
    return (a, b . (:) stmt)
contentToStmt (vars, stmts) (ContentVar d) = do
    (vars', d', stmts') <- bindDeref vars d
    oh <- [|outputHtml|]
    let stmt = NoBindS $ oh `AppE` d'
    return (vars', stmts . stmts' . (:) stmt)
contentToStmt (vars, stmts) (ContentUrl hasParams d) = do
    (vars', d', stmts') <- bindDeref vars d
    ou <- if hasParams then [|outputUrlParams|] else [|outputUrl|]
    let stmt = NoBindS $ ou `AppE` d'
    return (vars', stmts . stmts' . (:) stmt)
contentToStmt (vars, stmts) (ContentEmbed d) = do
    (vars', d', stmts') <- bindDeref vars d
    oe <- [|outputEmbed|]
    let stmt = NoBindS $ oe `AppE` d'
    return (vars', stmts . stmts' . (:) stmt)

liftConds :: Scope
          -> [(Deref, [Doc])]
          -> (Exp -> Exp)
          -> Q Exp
liftConds _vars [] front = do
    nil <- [|[]|]
    return $ front nil
liftConds vars ((bool, doc):conds) front = do
    let (base, rest) = shortestPath vars bool
    bool' <- identsToVal False (Deref rest) base
    (_, doc') <- foldM docToStmt (vars, id) doc
    doe <- safeDoE $ doc' []
    let pair = TupE [bool', doe]
    cons <- [|(:)|]
    let front' rest' = front (cons `AppE` pair `AppE` rest')
    liftConds vars conds front'

-- | Calls 'hamletWithSettings' with 'defaultHamletSettings'.
hamlet :: QuasiQuoter
hamlet = hamletWithSettings defaultHamletSettings

-- | Calls 'hamletWithSettings' using XHTML 1.0 Strict settings.
xhamlet :: QuasiQuoter
xhamlet = hamletWithSettings $ HamletSettings doctype True where
    doctype =
      "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" " ++
      "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"

-- | A quasi-quoter that converts Hamlet syntax into a function of form:
--
-- argument -> Hamlet url m ()
--
-- Please see accompanying documentation for a description of Hamlet syntax.
-- You must ensure that the type of m, url and argument all work properly with
-- the functions referred to in the template. Of course, worst case scenario is
-- the compiler will catch your mistakes.
hamletWithSettings :: HamletSettings -> QuasiQuoter
hamletWithSettings set =
    QuasiQuoter go $ error "Cannot quasi-quote Hamlet to patterns"
  where
    go s = do
      case parseDoc set s of
        Error s' -> error s'
        Ok d -> docsToExp d

-- deref helper funcs
shortestPath :: Scope
             -> Deref -- ^ path sought
             -> (Exp, [Ident]) -- ^ (base, path from base)
shortestPath scope (Deref is) = findMatch' svars
  where
    svars = sortBy (\(x, _) (y, _)
                  -> compare (length y) (length x)) scope
    findMatch' [] =
        case is of
            (top:rest) -> (getBase scope top, rest)
            [] -> error "shortestPath called with null deref"
    findMatch' (x:xs) =
        case checkMatch x of
            Just y -> y
            Nothing -> findMatch' xs
    checkMatch :: ([Ident], Exp) -> Maybe (Exp, [Ident])
    checkMatch (a, e')
        | a `isPrefixOf` is = Just (e', drop (length a) is)
        | otherwise = Nothing

-- | Converts a chain of idents and initial 'Exp' to a monadic value.
identsToVal :: Bool -- ^ is initial monadic?
            -> Deref
            -> Exp
            -> Q Exp
identsToVal isMonad (Deref []) e =
    if isMonad
        then return e
        else do
            ret <- [|return|]
            return $ ret `AppE` e
identsToVal isInitMonad (Deref (Ident i:is)) e = do
            let e' = VarE (mkName i) `AppE` e
             in identsToVal False (Deref is) e'

getBase :: Scope
        -> Ident
        -> Exp
getBase _ (Ident "") = error "Invalid empty ident"
getBase _ (Ident name@(x:_))
    | isUpper x = ConE $ mkName name
getBase scope (Ident top) =
    case lookup [Ident top] scope of
        Nothing -> VarE $ mkName top
        Just e -> e

bindMonadicVar :: String -> Exp -> Q (Exp, [Stmt] -> [Stmt])
bindMonadicVar name e = do
    lh <- error "FIXME" -- [|liftHamlet|]
    name' <- newName name
    let stmt = BindS (VarP name') $ lh `AppE` e
    return (VarE name', (:) stmt)

-- | Add a new binding for a 'Deref'
bindDeref :: Scope
          -> Deref
          -> Q (Scope, Exp, [Stmt] -> [Stmt])
bindDeref _ (Deref []) = error "bindDeref with null deref"
bindDeref scopeFirst (Deref ((Ident base):rest)) = do
    let base' = getBase scopeFirst $ Ident base
    (base'', stmts) <- return (base', id) -- FIXME
    (x, y, z, _) <- foldM go (scopeFirst, base'', stmts, [Ident base]) rest
    return (x, y, z)
  where
    go :: (Scope, Exp, [Stmt] -> [Stmt], [Ident])
       -> Ident
       -> Q (Scope, Exp, [Stmt] -> [Stmt], [Ident])
    go (scope, base', stmts, front) (Ident func) = do
        let (x, func') =
                if not (null func) && isUpper (head func)
                    then (ConE, "var" ++ func)
                    else (VarE, func)
        let rhs = x (mkName func) `AppE` base'
        name <- newName func'
        let stmt = LetS [FunD name [ Clause [] (NormalB rhs) []
                                   ]]
        let front' = front ++ [Ident func]
        return ((front', VarE name) : scope, VarE name,
                stmts . (:) stmt, front')
