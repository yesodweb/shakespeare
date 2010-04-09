{-# LANGUAGE RankNTypes #-}
module Text.Hamlet.Monad
    where

import Data.Text (Text, pack)
import Control.Applicative
import Control.Monad
import Web.Encodings

type Iteratee val seed m = seed -> val -> m (Either seed seed)
newtype Enumerator val m = Enumerator
    { runEnumerator :: forall seed.
        Iteratee val seed m -> seed
     -> m (Either seed seed)
    }

fromList :: Monad m => [a] -> Enumerator a m
fromList l = Enumerator $ go l where
    go [] _ seed = return $ Right seed
    go (l:ls) iter seed = do
        ea <- iter seed l
        case ea of
            Left seed' -> return $ Left seed
            Right seed' -> go ls iter seed'

newtype Hamlet url seed m a = Hamlet
    { runHamlet ::
       (url -> String)
    -> seed
    -> Iteratee Text seed m
    -> m (Either seed (a, seed))
    }

instance Monad m => Monad (Hamlet url seed m) where
    return x = Hamlet $ \_ seed _ -> return (Right (x, seed))
    (Hamlet f) >>= g = Hamlet go where
        go a c d = f a c d >>= go' a d
        go' _ _ (Left seed) = return $ Left seed
        go' a d (Right (v, seed)) = runHamlet (g v) a seed d
instance Monad m => Functor (Hamlet url seed m) where
    fmap = liftM
instance Monad m => Applicative (Hamlet url seed m) where
    pure = return
    (<*>) = ap

output :: Monad m => Text -> Hamlet url seed m ()
output bs = Hamlet go where
    go _ seed iter = do
        ea <- iter seed bs
        case ea of
            Left seed' -> return $ Left seed'
            Right seed' -> return $ Right ((), seed')

data Html = Encoded Text | Unencoded Text

outputHtml :: Monad m => Html -> Hamlet url seed m ()
outputHtml (Encoded t) = output t
outputHtml (Unencoded t) = output $ encodeHtml t

outputString :: Monad m => String -> Hamlet url seed m ()
outputString = output . pack

outputUrl :: Monad m => url -> Hamlet url seed m ()
outputUrl u = showUrl u >>= outputString

showUrl :: Monad m => url -> Hamlet url seed m String
showUrl url = Hamlet $ \s seed _ -> return (Right (s url, seed))

liftHamlet :: Monad m => m a -> Hamlet url seed m a
liftHamlet m = Hamlet $ \_ c _ -> m >>= \m' -> return (Right (m', c))

mapH :: Monad m
     => (val -> Hamlet url seed m ())
     -> Enumerator val m
     -> Hamlet url seed m ()
mapH each (Enumerator e) = Hamlet go where
    go surl seed iter = do
        res <- e (iter' surl iter) seed
        case res of
            Left seed' -> return $ Left seed'
            Right seed' -> return $ Right ((), seed')
    iter' surl iter seed val = do
        res <- runHamlet (each val) surl seed iter
        case res of
            Left seed' -> return $ Left seed'
            Right ((), seed') -> return $ Right seed'

condH :: Monad m
      => [(Hamlet url seed m Bool, Hamlet url seed m ())]
      -> Maybe (Hamlet url seed m ())
      -> Hamlet url seed m ()
condH [] Nothing = return ()
condH [] (Just x) = x
condH ((x, y):rest) z = do
    x' <- x
    if x' then y else condH rest z
