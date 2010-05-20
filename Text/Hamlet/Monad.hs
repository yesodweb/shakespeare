{-# LANGUAGE RankNTypes #-}
module Text.Hamlet.Monad
    ( -- * Generalized enumerator
      Iteratee
    , Enumerator (..)
    , fromList
      -- * Datatypes
    , Hamlet (..)
    , HtmlContent (..)
      -- * Output
    , output
    , outputHtml
    , outputString
    , outputUrl
    , outputUrlParams
    , outputEmbed
      -- * Utility functions
    , htmlContentToText
    , showUrl
    , liftHamlet
    , mapH
    , condH
    , maybeH
    , maybeH'
    , printHamlet
    , hamletToText
    , cdata
    ) where

import Data.Text (Text, pack)
import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as T
import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.List

-- | Something to be run for each val. Returns 'Left' when enumeration should
-- terminate immediately, 'Right' when it can receive more input.
type Iteratee val seed m = seed -> val -> m (Either seed seed)

-- | Generates a stream of values to be passed to an 'Iteratee'.
newtype Enumerator val m = Enumerator
    { runEnumerator :: forall seed.
        Iteratee val seed m -> seed
     -> m (Either seed seed)
    }

-- | Convert a list into an 'Enumerator'.
fromList :: Monad m => [a] -> Enumerator a m
fromList x = Enumerator $ go x where
    go [] _ seed = return $ Right seed
    go (l:ls) iter seed = do
        ea <- iter seed l
        case ea of
            Left seed' -> return $ Left seed'
            Right seed' -> go ls iter seed'

-- | 'Hamlet' is a monad that has two features:
--
-- * It passes along a function to convert a URL to a 'String'.
--
-- * It keeps an 'Iteratee' and a seed value so that it can output values.
-- Output is all done through a strict 'Text' value.
--
-- The URL to String function makes it very convenient to write templates
-- without knowing the absolute URLs for all referenced resources. For more
-- information on this approach, please see the web-routes package.
--
-- For efficiency, the 'Hamlet' monad halts execution as soon as the underlying
-- 'Iteratee' returns a 'Left' value. This is normally what you want; this
-- might cause a problem if you are relying on the side effects of a 'Hamlet'
-- action. However, it is not recommended to rely on side-effects. Though a
-- 'Hamlet' monad may perform IO actions, this should only be used for
-- read-only behavior for efficiency.
newtype Hamlet url m a = Hamlet
    { runHamlet :: forall seed.
       (url -> String)
    -> seed
    -> Iteratee Text seed m
    -> m (Either seed (a, seed))
    }

instance Monad m => Monad (Hamlet url m) where
    return x = Hamlet $ \_ seed _ -> return (Right (x, seed))
    (Hamlet f) >>= g = Hamlet go where
        go a c d = f a c d >>= go' a d
        go' _ _ (Left seed) = return $ Left seed
        go' a d (Right (v, seed)) = runHamlet (g v) a seed d
instance Monad m => Functor (Hamlet url m) where
    fmap = liftM
instance Monad m => Applicative (Hamlet url m) where
    pure = return
    (<*>) = ap

-- | Directly output strict 'Text' without any escaping.
output :: Monad m => Text -> Hamlet url m ()
output bs = Hamlet go where
    go _ seed iter = do
        ea <- iter seed bs
        case ea of
            Left seed' -> return $ Left seed'
            Right seed' -> return $ Right ((), seed')

-- | Content for an HTML document. 'Encoded' content should not be entity
-- escaped; 'Unencoded' should be.
data HtmlContent = Encoded Text | Unencoded Text
    deriving (Eq, Show, Read)
instance Monoid HtmlContent where
    mempty = Encoded mempty
    mappend x y = Encoded $ mappend (htmlContentToText x)
                                    (htmlContentToText y)

-- | Wrap some 'HtmlContent' for embedding in an XML file.
cdata :: HtmlContent -> HtmlContent
cdata h = mconcat
    [ Encoded $ pack "<![CDATA["
    , h
    , Encoded $ pack "]]>"
    ]

-- | Outputs the given 'HtmlContent', entity encoding any 'Unencoded' data.
outputHtml :: Monad m => HtmlContent -> Hamlet url m ()
outputHtml = output . htmlContentToText

-- | 'pack' a 'String' and call 'output'; this will not perform any escaping.
outputString :: Monad m => String -> Hamlet url m ()
outputString = output . pack

-- | Uses the URL rendering function to convert the given URL to a 'String' and
-- then calls 'outputString'.
outputUrl :: Monad m => url -> Hamlet url m ()
outputUrl u = showUrl u >>= outputString

-- | Same as 'outputUrl', but appends a query-string with given keys and
-- values.
outputUrlParams :: Monad m => (url, [(String, String)]) -> Hamlet url m ()
outputUrlParams (u, []) = outputUrl u
outputUrlParams (u, params) = do
    outputUrl u
    outputString $ showParams params
  where
    showParams x = '?' : intercalate "&" (map go x)
    go (x, y) = go' x ++ '=' : go' y
    go' = concatMap encodeUrlChar

-- | Taken straight from web-encodings; reimplemented here to avoid extra
-- dependencies.
encodeUrlChar :: Char -> String
encodeUrlChar c
    -- List of unreserved characters per RFC 3986
    -- Gleaned from http://en.wikipedia.org/wiki/Percent-encoding
    | 'A' <= c && c <= 'Z' = [c]
    | 'a' <= c && c <= 'z' = [c]
    | '0' <= c && c <= '9' = [c]
encodeUrlChar c@'-' = [c]
encodeUrlChar c@'_' = [c]
encodeUrlChar c@'.' = [c]
encodeUrlChar c@'~' = [c]
encodeUrlChar ' ' = "+"
encodeUrlChar y =
    let (a, c) = fromEnum y `divMod` 16
        b = a `mod` 16
        showHex' x -- FIXME just use Numeric version?
            | x < 10 = toEnum $ x + (fromEnum '0')
            | x < 16 = toEnum $ x - 10 + (fromEnum 'A')
            | otherwise = error $ "Invalid argument to showHex: " ++ show x
     in ['%', showHex' b, showHex' c]

-- | Only really used to ensure that the argument has the right type.
outputEmbed :: Monad m => Hamlet url m () -> Hamlet url m ()
outputEmbed = id

-- | Use the URL to 'String' rendering function to convert a URL to a 'String'.
showUrl :: Monad m => url -> Hamlet url m String
showUrl url = Hamlet $ \s seed _ -> return (Right (s url, seed))

-- | Lift a monadic action into the 'Hamlet' monad.
liftHamlet :: Monad m => m a -> Hamlet url m a
liftHamlet m = Hamlet $ \_ c _ -> m >>= \m' -> return (Right (m', c))

-- | Perform the given 'Hamlet' action for all values generated by the given
-- 'Enumerator'.
mapH :: Monad m
     => (val -> Hamlet url m ())
     -> Enumerator val m
     -> Hamlet url m ()
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

-- | Checks for truth in the left value in each pair in the first argument. If
-- a true exists, then the corresponding right action is performed. Only the
-- first is performed. In there are no true values, then the second argument is
-- performed, if supplied.
condH :: Monad m
      => [(m Bool, Hamlet url m ())]
      -> Maybe (Hamlet url m ())
      -> Hamlet url m ()
condH [] Nothing = return ()
condH [] (Just x) = x
condH ((x, y):rest) z = do
    x' <- liftHamlet x
    if x' then y else condH rest z

-- | Runs the second argument with the value in the first, if available.
maybeH :: Monad m
       => Maybe v
       -> (v -> Hamlet url m ())
       -> Hamlet url m ()
maybeH Nothing _ = return ()
maybeH (Just v) f = f v

-- | Runs the second argument with the value in the first, if available.
-- Otherwise, runs the third argument, if available.
maybeH' :: Monad m
        => Maybe v
        -> (v -> Hamlet url m ())
        -> Maybe (Hamlet url m ())
        -> Hamlet url m ()
maybeH' Nothing _ Nothing = return ()
maybeH' Nothing _ (Just x) = x
maybeH' (Just v) f _ = f v

-- | Prints a Hamlet to standard out. Good for debugging.
printHamlet :: (url -> String) -> Hamlet url IO () -> IO ()
printHamlet render h = runHamlet h render () iter >> return () where
    iter () text = do
        T.putStr text
        return $ Right ()

-- | Converts a 'Hamlet' to lazy text, using strict I/O.
hamletToText :: Monad m => (url -> String) -> Hamlet url m () -> m L.Text
hamletToText render h = do
    Right ((), front) <- runHamlet h render id iter
    return $ L.fromChunks $ front []
  where
    iter front text = return $ Right $ front . (:) text

-- | Returns HTML-ready text (ie, all entities are escaped properly).
htmlContentToText :: HtmlContent -> Text
htmlContentToText (Encoded t) = t
htmlContentToText (Unencoded t) = S.concatMap (pack . encodeHtmlChar) t

encodeHtmlChar :: Char -> String
encodeHtmlChar '<' = "&lt;"
encodeHtmlChar '>' = "&gt;"
encodeHtmlChar '&' = "&amp;"
encodeHtmlChar '"' = "&quot;"
encodeHtmlChar '\'' = "&#39;"
encodeHtmlChar c = [c]
