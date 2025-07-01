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

class Lift master => YesodSubApp master where

data SubApp master

data Test a b

spec :: Monad m => m ()
spec = return ()

mkMessage "(YesodSubApp master) => SubApp master" "other-messages" "en" 

mkMessage "Test a b" "test-messages" "en"
