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

data Test = Test

class Testable a where
  isTestable :: a -> Bool 

instance Testable Test where 
  isTestable Test = True

spec :: Monad m => m ()
spec = return ()

mkMessage "(YesodSubApp master) => SubApp master" "other-messages" "en" 

mkMessage "Test" "test-messages" "en"

newtype SubAppNoRec master = SubAppNoRec
  {
     getOrderingNR :: Ordering
  }

data TestNoRec = TestNoRec

instance Testable TestNoRec where
  isTestable TestNoRec = True

mkMessageOpts
  (setConPrefix "MsgNR" $ setUseRecordCons False defMakeMessageOpts)
  "(YesodSubApp master) => SubAppNoRec master"
  "(YesodSubApp master) => SubAppNoRec master"
  "other-messages"
  "en"

mkMessageOpts
  (setConPrefix "MsgNR" $ setUseRecordCons False defMakeMessageOpts)
  "TestNoRec"
  "TestNoRec"
  "test-messages"
  "en"

