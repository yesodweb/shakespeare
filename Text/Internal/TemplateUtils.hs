{-# LANGUAGE TemplateHaskell #-}
module Text.Internal.TemplateUtils  where

import System.Directory
import System.FilePath
import Text.Shakespeare.Base (readFileUtf8)
import Language.Haskell.TH.Syntax

-- | Uses 'addDependentFile' on a file relative to the src root
-- to mark it as being checked for changes when compiled with TemplateHaskell.
--
-- Returns an empty list of declarations so that it can be used with:
--
-- >$(addDependentFileRelative "MyDependency.txt")
-- move it to: Language.Haskell.TH.Syntax
addDependentFileRelative :: FilePath -> Q [Dec]
addDependentFileRelative relativeFile = do
    pwd <- runIO getCurrentDirectory
    addDependentFile $ pwd </> relativeFile
    return []

-- | Track file via ghc dependencies and return IO returning file contents
readFileQ :: FilePath -> Q String
readFileQ fp = addDependentFileRelative fp >> qRunIO (readFileUtf8 fp)
