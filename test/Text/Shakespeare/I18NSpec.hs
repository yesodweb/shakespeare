{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Text.Shakespeare.I18NSpec
    ( spec
    ) where

import           Data.Text             (Text)
import           Text.Shakespeare.I18N
import           Language.Haskell.TH.Syntax (Lift(..)) 

spec :: Monad m => m ()
spec = return ()

class Lift master => YesodSubApp master where
   data YesodSubAppData master :: *

newtype SubApp master = SubApp
  {
     getOrdering :: Ordering
  }

mkMessage "(YesodSubApp master) => SubApp master" "test-messages" "en" 

