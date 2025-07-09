{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Text.Shakespeare.I18NSpec
    ( spec
    ) where

import           Data.Text             (Text)
import           Text.Shakespeare.I18N
import           Test.Hspec

class YesodSubApp master where

instance YesodSubApp ()

newtype SubApp master = SubApp master

instance YesodSubApp (SubApp master)

data Test a b

mkMessage "(YesodSubApp master) => SubApp master" "other-messages" "en" 

mkMessage "Test a b" "test-messages" "en"

newtype SubAppNoRec master = SubAppNoRec master

instance YesodSubApp (SubAppNoRec master)

data TestNoRec

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

spec :: Spec
spec = do
  describe "I18N" $ do
    it "should generate messages with record constructors" $ do
      let msg = MsgEntryCreatedRR { subAppMessageTitle = "foo" }
      renderMessage (SubApp ()) [] msg `shouldBe` "Your new blog post, foo, has been created"

    it "should generate messages without record constructors" $ do
      let msg = MsgNREntryCreatedRR "bar"
      renderMessage (SubAppNoRec ()) [] msg `shouldBe` "Your new blog post, bar, has been created"
