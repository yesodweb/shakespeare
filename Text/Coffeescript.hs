{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}

module Text.Coffeescript
    ( 
      ToCoffee (..)
    , Coffee
    , coffee
    , coffeeFile
    , coffeeFileDebug
    , renderCoffee
    ) where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText, fromLazyText)
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Text.YesodTemplate
import System.Process (readProcess, createProcess, CreateProcess(..), proc, waitForProcess, std_out, StdStream(..))
import Data.Text.IO (hGetContents)
{-import Data.Char (isSpace)-}
  {-where-}
  {-rstrip = reverse . dropWhile isSpace . reverse-}

{--}
import Debug.Trace
debug :: (Show a) => a -> a
debug a = trace ("DEBUG: " ++ show a) a
--}

type Coffee url = YesodEnv url -> Builder


{-
-- Print out any errors
run :: FilePath -> [String] -> IO TL.Text
run path args = do
  (_,Just hStdout,Just hErr ,pHandle) <- createProcess (proc path args) {
      std_out = CreatePipe, std_err = CreatePipe
    }
  r <- hGetContents hStdout
  if TS.null r
    then do mapM_ putStrLn (path:args)
            e <- hGetContents hErr
            putStrLn $ TS.unpack e
    else    return ()
  code <- waitForProcess pHandle
  return $ r 
-}

-- nead to get a handle and use
-- hGetContents
renderCoffee :: YesodEnv url -> Coffee url -> IO TL.Text
renderCoffee r s = do
  out <- readProcess "coffee" ["-epb", TL.unpack $ toLazyText $ s r] []
  return $ TL.pack out

-- the types that can be placed in a template
class Lift c => ToCoffee c where
    toCoffee :: c -> Builder
instance ToCoffee [Char]  where toCoffee = fromLazyText . TL.pack
instance ToCoffee TS.Text where toCoffee = fromText
instance ToCoffee TL.Text where toCoffee = fromLazyText

coffeeTemplate :: YesodTemplate
coffeeTemplate = coffeeYesodTemplate {toBuilder = toCoffeeExp}
  where toCoffeeExp = [|toCoffee|]

coffee :: QuasiQuoter
coffee = QuasiQuoter { quoteExp = stringToTH coffeeTemplate }

coffeeFile :: FilePath -> Q Exp
coffeeFile fp = file coffeeTemplate fp

coffeeFileDebug :: FilePath -> Q Exp
coffeeFileDebug fp = fileDebug coffeeTemplate fp
