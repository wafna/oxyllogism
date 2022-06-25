{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Main (main) where

--------------------------------------------------------------------------------------------------
-- Demonstrating the stacking of monads.
-- Note that the two test functions can be used in either stack due to their flexibly constrained definitions.
-- https://stackoverflow.com/questions/31838776/whats-the-difference-between-statet-s-exceptt-e-m-and-exceptt-e-statet-s-m

import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import Data.Functor.Identity

test1 :: (MonadState Int m, MonadError String m) => m Bool
test1 = do
    put 1
    _ <- throwError "foobar"
    put 2
    return False

test2 :: (Alternative m, MonadState Int m, MonadError String m) => m Bool
test2 = do
    put 4
    test1 <|> return True

runStateExceptT :: Monad m => s -> ExceptT e (StateT s m) a -> m (Either e a, s)
runStateExceptT s = flip runStateT s . runExceptT

runExceptStateT :: Monad m => s -> StateT s (ExceptT e m) a -> m (Either e (a, s))
runExceptStateT s = runExceptT . flip runStateT s

--------------------------------------------------------------------------------------------------
-- Example of IO with monad transformers.

type MyState = Int
type MyError = String

type MyMonadSE a = ExceptT MyError (StateT MyState IO) a

runMyMonadSE :: MyState -> MyMonadSE a -> IO (Either MyError a, MyState)
runMyMonadSE initState action = liftIO $ runStateT (runExceptT action) initState

type MyMonadES a = StateT MyState (ExceptT MyError IO) a

runMyMonadES :: MyState -> MyMonadES a -> IO (Either MyError (a, MyState))
runMyMonadES initState action = liftIO $ runExceptT $ runStateT action initState

-- Most general type signature.
-- Works with either monad.
worksInBoth :: (MonadState MyState m, MonadError MyError m, MonadIO m) => m String
worksInBoth = do
    get >>= liftIO . print
    return "Works in either monad."

seOK :: MyMonadSE String
seOK = do
    get >>= liftIO . print
    return "se ok"

seBarf :: MyMonadSE ()
seBarf = throwError "es error"

esOK :: MyMonadES String
esOK = do
    get >>= liftIO . print
    return "es ok"

esBarf :: MyMonadES ()
esBarf = throwError "es error"

main :: IO ()
main = do
    -- These show the difference in the nesting of the monads.
    print $ runIdentity . runStateExceptT 3 $ test1
    print $ runIdentity . runExceptStateT 3 $ test1
    print $ runIdentity . runStateExceptT 3 $ test2
    print $ runIdentity . runExceptStateT 3 $ test2
    -- Some stuff with IO
    (runMyMonadSE 42 $ do
            liftIO $ putStrLn "woohoo"
            modify $ (+) 86)
        >>= print
    putStrLn "IO Monads..."
    runMyMonadSE 42 worksInBoth >>= print
    runMyMonadSE 42 seOK >>= print
    runMyMonadSE 42 seBarf >>= print
    runMyMonadES 42 worksInBoth >>= print
    runMyMonadES 42 esOK >>= print
    runMyMonadES 42 esBarf >>= print
