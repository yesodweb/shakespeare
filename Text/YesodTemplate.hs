{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

module Text.YesodTemplate (
    YesodEnv 
  , YesodTemplate (..), defaultYesodTemplate
  , stringToTH
  , file
  , fileDebug
  , Content (..)
  , readFileQ
  , coffeeYesodTemplate
  , readUtf8File
) where

import Text.ParserCombinators.Parsec hiding (Line)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Data.Monoid
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText, fromLazyText)
import System.IO.Unsafe (unsafePerformIO)
import Text.Hamlet.Quasi (readUtf8File)
import Text.Shakespeare

-- move to Shakespeare?
readFileQ :: FilePath -> Q [Char]
readFileQ fp = do
    qRunIO $ readFileUtf8 fp

readFileUtf8 :: FilePath -> IO String
readFileUtf8 fp = fmap TL.unpack $ readUtf8File fp

instance Lift TS.Text where lift = lift . TS.unpack
instance Lift TL.Text where lift = lift . TL.unpack

instance Lift Builder where lift = lift . toLazyText

type YesodEnv url = (url -> [(String, String)] -> String)

-- variables within scope of template
data VarType = VTPlain | VTUrl | VTUrlParam | VTMixin

data Content = ContentRaw [Char]
             | ContentVar Deref
             | ContentUrl Deref
             | ContentUrlParam Deref
             | ContentMix Deref
    deriving (Show, Eq)

instance Lift Content where
  lift (ContentRaw str)    = liftContent [|ContentRaw|] str
  lift (ContentVar d)      = liftContent [|ContentVar|] d
  lift (ContentUrl d)      = liftContent [|ContentVar|] d
  lift (ContentUrlParam d) = liftContent [|ContentVar|] d
  lift (ContentMix d)      = liftContent [|ContentVar|] d

liftContent ceq arg = do
  ce <- ceq
  ae <- lift arg
  return $ ce `AppE` ae

-- template expression variable
data Monoid a => VarExp url a = EPlain Builder
                              | EUrl url
                              | EUrlParam (url, [(String, String)])
                              | EMixin (YesodEnv url -> a)

data YesodTemplate = YesodTemplate {
    builderWrapper :: Q Exp
  , toBuilder :: Q Exp
  , startVar :: Char
}

defaultYesodTemplate = YesodTemplate {
  startVar = '#'
}
coffeeYesodTemplate = YesodTemplate {
  startVar = '%'
}

contentFromString :: YesodTemplate -> (String -> [Content])
contentFromString yt = contentFromStringStart (startVar yt)

contentFromStringStart :: Char -> String -> [Content]
contentFromStringStart startChar s = do
    compressContents $ either (error . show) id $ parse parseContents s s
    where
      parseContents :: Parser [Content]
      parseContents = many1 $ (parseHash' startChar) <|> parseAt' <|> parseCaret' <|> parseChar startChar
      compressContents :: [Content] -> [Content]
      compressContents [] = []
      compressContents (ContentRaw x:ContentRaw y:z) =
          compressContents $ ContentRaw (x ++ y) : z
      compressContents (x:y) = x : compressContents y

file :: YesodTemplate -> FilePath -> Q Exp
file yt fp = readFileQ fp >>= stringToTH yt

stringToTH :: YesodTemplate -> String ->  Q Exp
stringToTH yt = 
  contentsToType . (contentFromString yt)
  where
    contentsToType :: [Content] -> Q Exp
    contentsToType a = do
        r <- newName "_render"
        bw <- builderWrapper yt
        c <- mapM (contentToTH r bw) a
        d <- case c of
                [] -> [|mempty|]
                [x] -> return x
                _ -> do
                    mc <- [|mconcat|]
                    return $ mc `AppE` ListE c
        return $ LamE [VarP r] d
      where
          toB = toBuilder yt
          contentToTH :: Name -> Exp -> Content -> Q Exp  
          contentToTH _ bw (ContentRaw s') = do
              ts <- [|fromText . TS.pack|]
              return $ bw `AppE` (ts `AppE` LitE (StringL s'))
          contentToTH _ bw (ContentVar d) = do
              tB <- toB
              return $ bw `AppE` (tB `AppE` derefToExp [] d)
          contentToTH r bw (ContentUrl d) = do
              ts <- [|fromText . TS.pack|]
              return $ bw `AppE` (ts `AppE` (VarE r `AppE` derefToExp [] d `AppE` ListE []))
          contentToTH r bw (ContentUrlParam d) = do
              ts <- [|fromText . TS.pack|]
              up <- [|\r' (u, p) -> r' u p|]
              return $ bw `AppE` (ts `AppE` (up `AppE` VarE r `AppE` derefToExp [] d))
          contentToTH r _ (ContentMix d) = do
              return $ derefToExp [] d `AppE` VarE r

fileDebug :: YesodTemplate -> FilePath -> Q Exp
fileDebug yt fp = do
    s <- readFileQ fp
    let b = concatMap getVars $ (contentFromString yt) s
    c <- mapM vtToExp b
    let sc = startVar yt
    bw <- builderWrapper yt
    rt <- [|runtime|]
    cr <- [|(contentFromStringStart sc)|]
    fp' <- [|fp|]
    return $ (rt `AppE` bw) `AppE` cr `AppE` fp' `AppE` ListE c
  where
    toB = toBuilder yt
    render = newName "_render"

    vtToExp :: (Deref, VarType) -> Q Exp
    vtToExp (d, vt) = do
        d' <- lift d
        c' <- c vt
        return $ TupE [d', c' `AppE` derefToExp [] d]
      where
        c :: VarType -> Q Exp
        c VTPlain = do r <- render
                       ep <- [|EPlain|]
                       tB <- toB
                       return $ LamE [VarP r] (ep `AppE` (tB `AppE` (VarE r)))
        c VTUrl = [|EUrl|]
        c VTUrlParam = [|EUrlParam|]
        c VTMixin = [|EMixin|]


parseHash' startChar = either ContentRaw ContentVar `fmap` parseHashStart startChar
parseAt' =
    either ContentRaw go `fmap` parseAt
  where
    go (d, False) = ContentUrl d
    go (d, True) = ContentUrlParam d

parseHashStart :: Char -> Parser (Either String Deref)
parseHashStart startChar = do
    _ <- char startChar
    (char '\\' >> return (Left [startChar])) <|> (do
        _ <- char '{'
        deref <- parseDeref
        _ <- char '}'
        return $ Right deref) <|> (do
            -- Check for hash just before newline
            _ <- lookAhead (oneOf "\r\n" >> return ()) <|> eof
            return $ Left ""
            ) <|> return (Left [startChar])


parseCaret' = either ContentRaw ContentMix `fmap` parseCaret
parseChar startChar =
            ContentRaw `fmap` (many1 $ noneOf (startChar:"@^"))

getVars :: Content -> [(Deref, VarType)]
getVars ContentRaw{} = []
getVars (ContentVar d) = [(d, VTPlain)]
getVars (ContentUrl d) = [(d, VTUrl)]
getVars (ContentUrlParam d) = [(d, VTUrlParam)]
getVars (ContentMix d) = [(d, VTMixin)]

runtime :: Monoid a => (Builder -> a) -> (String -> [Content]) -> FilePath -> [(Deref, VarExp url a)] -> (YesodEnv url -> a)
runtime bw cfs fp cd render' = unsafePerformIO $ do
    s <- readFileUtf8 fp
    return $ mconcat $ map contentToBuilder $ cfs s
  where
    contentToBuilder (ContentRaw s) = bw $ fromText $ TS.pack s
    contentToBuilder (ContentVar d) =
        case lookup d cd of
            Just (EPlain s) -> bw s
            _ -> error $ show d ++ ": expected EDPlain"
    contentToBuilder (ContentUrl d) =
        case lookup d cd of
            Just (EUrl u) -> bw $ fromText $ TS.pack $ render' u []
            _ -> error $ show d ++ ": expected EUrl"
    contentToBuilder (ContentUrlParam d) =
        case lookup d cd of
            Just (EUrlParam (u, p)) ->
                bw $ fromText $ TS.pack $ render' u p
            _ -> error $ show d ++ ": expected EUrlParam"
    contentToBuilder (ContentMix d) =
        case lookup d cd of
            Just (EMixin m) -> m render'
            _ -> error $ show d ++ ": expected EMixin"
