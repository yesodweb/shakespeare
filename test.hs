{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Text.Hamlet.Quasi
import Text.Hamlet.Monad
import Data.Text (pack, unpack)

arg = "argument"
something = const $ return $ Unencoded $ pack "<something>"
another = const $ return 8

foo = [$hamlet|
#wrapper
  .bar
    %span!baz=bin
      Hello World!!!
      $something$
      %a!href=@another@ link content
    %ul
        $forall getList entry
            %li $entry$
    $if false
        ignored
    $elseif false2
        also ignored
    $elseif true
        this is print out
    $else
        ignored again
|]

getList :: (Monad n) => String -> n (Enumerator Html IO)
getList = return . fromList . map go where
    go = Unencoded . pack . return

main = runHamlet (foo arg) showUrl' () printI

showUrl' :: Int -> String
showUrl' i = show $ i + 5

printI () text = do
    --putStrLn "\n\nAnother batch:"
    putStr $ unpack text
    return $ Right ()

false = const $ return False
false2 = const $ return False
true = const $ return True
