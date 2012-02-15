{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Shakespeare.I18N
-- Copyright   :  2012 Michael Snoyman <michael@snoyman.com>, Jeremy Shaw
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  Michael Snoyman <michael@snoyman.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a type-based system for providing translations
-- for text strings.
--
-- It is similar in purpose to gettext or Java message bundles.
--
-- The core idea is to create simple data type where each constructor
-- represents a phrase, sentence, paragraph, etc. For example:
--
-- > data AppMessages = Hello | Goodbye
--
-- The 'RenderMessage' class is used to retrieve the appropriate
-- translation for a message value:
--
-- > class RenderMessage master message where
-- >   renderMessage :: master  -- ^ type that specifies which set of translations to use
-- >                 -> [Lang]  -- ^ acceptable languages in descending order of preference
-- >                 -> message -- ^ message to translate
-- >                 -> Text
--
-- Defining the translation type and providing the 'RenderMessage'
-- instance in Haskell is not very translator friendly. Instead,
-- translations are generally provided in external translations
-- files. Then the 'mkMessage' Template Haskell function is used to
-- read the external translation files and automatically create the
-- translation type and the @RenderMessage@ instance.
--
-- A full description of using this module to create translations for @Hamlet@ can be found here:
--
--  <http://www.yesodweb.com/book/internationalization>
--
-- A full description of using the module to create translations for @HSP@ can be found here:
--
--  <http://happstack.com/docs/crashcourse/Templates.html#hsp-i18n>
--
-- You can also adapt those instructions for use with other systems.
module Text.Shakespeare.I18N
    ( mkMessage
    , mkMessageFor
    , mkMessageVariant
    , RenderMessage (..)
    , ToMessage (..)
    , SomeMessage (..)
    , Lang
    ) where

import Language.Haskell.TH.Syntax
import Data.Text (Text, pack, unpack)
import System.Directory
import Data.Maybe (catMaybes)
import Data.List (isSuffixOf, sortBy, foldl')
import qualified Data.ByteString as S
import Data.Text.Encoding (decodeUtf8)
import Data.Char (isSpace, toLower, toUpper)
import Data.Ord (comparing)
import Text.Shakespeare.Base (Deref (..), Ident (..), parseHash, derefToExp)
import Text.ParserCombinators.Parsec (parse, many, eof, many1, noneOf, (<|>))
import Control.Arrow ((***))
import Data.Monoid (mempty, mappend)
import qualified Data.Text as T
import Data.String (IsString (fromString))

-- | 'ToMessage' is used to convert the value inside #{ } to 'Text'
--
-- The primary purpose of this class is to allow the value in #{ } to
-- be a 'String' or 'Text' rather than forcing it to always be 'Text'.
class ToMessage a where
    toMessage :: a -> Text
instance ToMessage Text where
    toMessage = id
instance ToMessage String where
    toMessage = Data.Text.pack

-- | the 'RenderMessage' is used to provide translations for a message types
--
-- The 'master' argument exists so that it is possible to provide more
-- than one set of translations for a 'message' type. This is useful
-- if a library provides a default set of translations, but the user
-- of the library wants to provide a different set of translations.
class RenderMessage master message where
    renderMessage :: master  -- ^ type that specifies which set of translations to use
                  -> [Lang]  -- ^ acceptable languages in descending order of preference
                  -> message -- ^ message to translate
                  -> Text

instance RenderMessage master Text where
    renderMessage _ _ = id

-- | an RFC1766 / ISO 639-1 language code (eg, @fr@, @en-GB@, etc).
type Lang = Text

-- |generate translations from translation files
--
-- This function will:
--
--  1. look in the supplied subdirectory for files ending in @.msg@
--
--  2. generate a type based on the constructors found
--
--  3. create a 'RenderMessage' instance
--
mkMessage :: String   -- ^ base name to use for translation type
          -> FilePath -- ^ subdirectory which contains the translation files
          -> Lang     -- ^ default translation language
          -> Q [Dec]
mkMessage dt folder lang =
    mkMessageCommon True "Msg" "Message" dt dt folder lang


-- | create 'RenderMessage' instance for an existing data-type
mkMessageFor :: String     -- ^ master translation data type
             -> String     -- ^ existing type to add translations for
             -> FilePath   -- ^ path to translation folder
             -> Lang       -- ^ default language
             -> Q [Dec]
mkMessageFor master dt folder lang = mkMessageCommon False "" "" master dt folder lang

-- | create an additional set of translations for a type created by `mkMessage`
mkMessageVariant :: String     -- ^ master translation data type
                 -> String     -- ^ existing type to add translations for
                 -> FilePath   -- ^ path to translation folder
                 -> Lang       -- ^ default language
                 -> Q [Dec]
mkMessageVariant master dt folder lang = mkMessageCommon False "Msg" "Message" master dt folder lang

-- |used by 'mkMessage' and 'mkMessageFor' to generate a 'RenderMessage' and possibly a message data type
mkMessageCommon :: Bool      -- ^ generate a new datatype from the constructors found in the .msg files
                -> String    -- ^ string to append to constructor names
                -> String    -- ^ string to append to datatype name
                -> String    -- ^ base name of master datatype
                -> String    -- ^ base name of translation datatype
                -> FilePath  -- ^ path to translation folder
                -> Lang      -- ^ default lang
                -> Q [Dec]
mkMessageCommon genType prefix postfix master dt folder lang = do
    files <- qRunIO $ getDirectoryContents folder
    contents <- qRunIO $ fmap catMaybes $ mapM (loadLang folder) files
    sdef <-
        case lookup lang contents of
            Nothing -> error $ "Did not find main language file: " ++ unpack lang
            Just def -> toSDefs def
    mapM_ (checkDef sdef) $ map snd contents
    let mname = mkName $ dt ++ postfix
    c1 <- fmap concat $ mapM (toClauses prefix dt) contents
    c2 <- mapM (sToClause prefix dt) sdef
    c3 <- defClause
    return $
     ( if genType 
       then ((DataD [] mname [] (map (toCon dt) sdef) []) :)
       else id)
        [ InstanceD
            []
            (ConT ''RenderMessage `AppT` (ConT $ mkName master) `AppT` ConT mname)
            [ FunD (mkName "renderMessage") $ c1 ++ c2 ++ [c3]
            ]
        ]

toClauses :: String -> String -> (Lang, [Def]) -> Q [Clause]
toClauses prefix dt (lang, defs) =
    mapM go defs
  where
    go def = do
        a <- newName "lang"
        (pat, bod) <- mkBody dt (prefix ++ constr def) (map fst $ vars def) (content def)
        guard <- fmap NormalG [|$(return $ VarE a) == pack $(lift $ unpack lang)|]
        return $ Clause
            [WildP, ConP (mkName ":") [VarP a, WildP], pat]
            (GuardedB [(guard, bod)])
            []

mkBody :: String -- ^ datatype
       -> String -- ^ constructor
       -> [String] -- ^ variable names
       -> [Content]
       -> Q (Pat, Exp)
mkBody dt cs vs ct = do
    vp <- mapM go vs
    let pat = RecP (mkName cs) (map (varName dt *** VarP) vp)
    let ct' = map (fixVars vp) ct
    pack' <- [|Data.Text.pack|]
    tomsg <- [|toMessage|]
    let ct'' = map (toH pack' tomsg) ct'
    mapp <- [|mappend|]
    let app a b = InfixE (Just a) mapp (Just b)
    e <-
        case ct'' of
            [] -> [|mempty|]
            [x] -> return x
            (x:xs) -> return $ foldl' app x xs
    return (pat, e)
  where
    toH pack' _ (Raw s) = pack' `AppE` SigE (LitE (StringL s)) (ConT ''String)
    toH _ tomsg (Var d) = tomsg `AppE` derefToExp [] d
    go x = do
        let y = mkName $ '_' : x
        return (x, y)
    fixVars vp (Var d) = Var $ fixDeref vp d
    fixVars _ (Raw s) = Raw s
    fixDeref vp (DerefIdent (Ident i)) = DerefIdent $ Ident $ fixIdent vp i
    fixDeref vp (DerefBranch a b) = DerefBranch (fixDeref vp a) (fixDeref vp b)
    fixDeref _ d = d
    fixIdent vp i =
        case lookup i vp of
            Nothing -> i
            Just y -> nameBase y

sToClause :: String -> String -> SDef -> Q Clause
sToClause prefix dt sdef = do
    (pat, bod) <- mkBody dt (prefix ++ sconstr sdef) (map fst $ svars sdef) (scontent sdef)
    return $ Clause
        [WildP, ConP (mkName "[]") [], pat]
        (NormalB bod)
        []

defClause :: Q Clause
defClause = do
    a <- newName "sub"
    c <- newName "langs"
    d <- newName "msg"
    rm <- [|renderMessage|]
    return $ Clause
        [VarP a, ConP (mkName ":") [WildP, VarP c], VarP d]
        (NormalB $ rm `AppE` VarE a `AppE` VarE c `AppE` VarE d)
        []

toCon :: String -> SDef -> Con
toCon dt (SDef c vs _) =
    RecC (mkName $ "Msg" ++ c) $ map go vs
  where
    go (n, t) = (varName dt n, NotStrict, ConT $ mkName t)

varName :: String -> String -> Name
varName a y =
    mkName $ concat [lower a, "Message", upper y]
  where
    lower (x:xs) = toLower x : xs
    lower [] = []
    upper (x:xs) = toUpper x : xs
    upper [] = []

checkDef :: [SDef] -> [Def] -> Q ()
checkDef x y =
    go (sortBy (comparing sconstr) x) (sortBy (comparing constr) y)
  where
    go _ [] = return ()
    go [] (b:_) = error $ "Extra message constructor: " ++ constr b
    go (a:as) (b:bs)
        | sconstr a < constr b = go as (b:bs)
        | sconstr a > constr b = error $ "Extra message constructor: " ++ constr b
        | otherwise = do
            go' (svars a) (vars b)
            go as bs
    go' ((an, at):as) ((bn, mbt):bs)
        | an /= bn = error "Mismatched variable names"
        | otherwise =
            case mbt of
                Nothing -> go' as bs
                Just bt
                    | at == bt -> go' as bs
                    | otherwise -> error "Mismatched variable types"
    go' [] [] = return ()
    go' _ _ = error "Mistmached variable count"

toSDefs :: [Def] -> Q [SDef]
toSDefs = mapM toSDef

toSDef :: Def -> Q SDef
toSDef d = do
    vars' <- mapM go $ vars d
    return $ SDef (constr d) vars' (content d)
  where
    go (a, Just b) = return (a, b)
    go (a, Nothing) = error $ "Main language missing type for " ++ show (constr d, a)

data SDef = SDef
    { sconstr :: String
    , svars :: [(String, String)]
    , scontent :: [Content]
    }

data Def = Def
    { constr :: String
    , vars :: [(String, Maybe String)]
    , content :: [Content]
    }

loadLang :: FilePath -> FilePath -> IO (Maybe (Lang, [Def]))
loadLang folder file = do
    let file' = folder ++ '/' : file
    e <- doesFileExist file'
    if e && ".msg" `isSuffixOf` file
        then do
            let lang = pack $ reverse $ drop 4 $ reverse file
            bs <- S.readFile file'
            let s = unpack $ decodeUtf8 bs
            defs <- fmap catMaybes $ mapM parseDef $ lines s
            return $ Just (lang, defs)
        else return Nothing

parseDef :: String -> IO (Maybe Def)
parseDef "" = return Nothing
parseDef ('#':_) = return Nothing
parseDef s =
    case end of
        ':':end' -> do
            content' <- fmap compress $ parseContent $ dropWhile isSpace end'
            case words begin of
                [] -> error $ "Missing constructor: " ++ s
                (w:ws) -> return $ Just Def
                            { constr = w
                            , vars = map parseVar ws
                            , content = content'
                            }
        _ -> error $ "Missing colon: " ++ s
  where
    (begin, end) = break (== ':') s

data Content = Var Deref | Raw String

compress :: [Content] -> [Content]
compress [] = []
compress (Raw a:Raw b:rest) = compress $ Raw (a ++ b) : rest
compress (x:y) = x : compress y

parseContent :: String -> IO [Content]
parseContent s =
    either (error . show) return $ parse go s s
  where
    go = do
        x <- many go'
        eof
        return x
    go' = (Raw `fmap` many1 (noneOf "#")) <|> (fmap (either Raw Var) parseHash)

parseVar :: String -> (String, Maybe String)
parseVar s =
    case break (== '@') s of
        (x, '@':y) -> (x, Just y)
        _ -> (s, Nothing)

data SomeMessage master = forall msg. RenderMessage master msg => SomeMessage msg

instance IsString (SomeMessage master) where
    fromString = SomeMessage . T.pack
