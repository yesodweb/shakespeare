{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Text.Shakespeare.I18NSpec
    ( spec
    ) where

import           Data.Text             (Text)
import           Text.Shakespeare.I18N

spec :: Monad m => m ()
spec = return ()

data Test = Test

mkMessage "Test" "test-messages" "en"
