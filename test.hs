{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Text.Hamlet.Quasi
import Text.Hamlet.Monad
import Data.Text

arg = "arg"
something = const $ return $ Unencoded $ pack "<something>"
another = const $ return 8

foo = [$hamlet|
#wrapper
  .bar
    %span!baz=bin
      Hello World!!!
      $something$
      %a!href=@another@ link content
|]

main = runHamlet foo showUrl' () printI

showUrl' :: Int -> String
showUrl' i = show $ i + 5

printI () text = do
    putStrLn "\n\nAnother batch:"
    putStrLn $ unpack text
    return $ Right ()
