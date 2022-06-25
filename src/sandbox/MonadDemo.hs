{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Main (main) where

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

-- Example of IO with monad transformers.

type MyState = Int
type MyError = String
type MyMonad a = StateT MyState (ExceptT MyError IO) a

runMyMonad :: MyState -> MyMonad a -> IO (Either MyError (a, MyState))
runMyMonad initState action = liftIO $ runExceptT $ runStateT action initState

myf1 :: MyMonad String
myf1 = return "F1!"

-- Most general type signature.
myf2 :: (MonadState MyState m, MonadError MyError m, MonadIO m) => m String
myf2 = 
    do
    s <- get
    liftIO $ print s
    return "F2!"

main :: IO ()
main = do
    print $ runIdentity . runStateExceptT 3 $ test1
    print $ runIdentity . runExceptStateT 3 $ test1
    print $ runIdentity . runStateExceptT 3 $ test2
    print $ runIdentity . runExceptStateT 3 $ test2
    foo <- runMyMonad 42 $ do
        liftIO $ putStrLn "woohoo"
        modify $ (+) 86
    print foo
    runMyMonad 42 myf1 >>= print
    runMyMonad 42 myf2 >>= print    
