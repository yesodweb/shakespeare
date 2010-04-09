{-# LANGUAGE RankNTypes #-}
module Text.Hamlet.Monad
    where

import Data.Text (Text, pack)
import Control.Applicative
import Control.Monad

type Iteratee val seed m = seed -> val -> m (Either seed seed)
newtype Enumerator val m = Enumerator
    { runEnumerator :: forall seed.
        Iteratee val seed m -> seed
     -> m (Either seed seed)
    }

newtype Hamlet url seed m a = Hamlet
    { unHamlet ::
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
        go' a d (Right (v, seed)) = unHamlet (g v) a seed d
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

outputString :: Monad m => String -> Hamlet url seed m ()
outputString = output . pack

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
        res <- unHamlet (each val) surl seed iter
        case res of
            Left seed' -> return $ Left seed'
            Right ((), seed') -> return $ Right seed'
