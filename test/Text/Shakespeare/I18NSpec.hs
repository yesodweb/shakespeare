{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Text.Shakespeare.I18NSpec
    ( spec
    ) where

import           Data.Text             (Text)
import           Text.Shakespeare.I18N
import           Language.Haskell.TH.Syntax
import           Test.Hspec

class Lift master => YesodSubApp master where
   data YesodSubAppData master :: *

newtype SubApp master = SubApp
  {
     getOrdering :: Ordering
  }

data Test a b = Test

class Testable a where
  isTestable :: a -> Bool 

instance Testable Test where 
  isTestable Test = True

spec :: Monad m => m ()
spec = return ()

mkMessage "(YesodSubApp master) => SubApp master" "other-messages" "en" 

mkMessage "Test a b" "test-messages" "en"

