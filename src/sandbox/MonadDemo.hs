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
myf1 :: (MonadState MyState m, MonadError MyError m, MonadIO m) => m String
myf1 = do
    get >>= liftIO . print
    return "Works in either monad."

mySE_1 :: MyMonadSE String
mySE_1 = do
    get >>= liftIO . print
    return "F1!"

mySE_2 :: MyMonadSE ()
mySE_2 = throwError "F3!"

myES_1 :: MyMonadES String
myES_1 = do
    get >>= liftIO . print
    return "F1!"

myES_2 :: MyMonadES ()
myES_2 = throwError "F3!"

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
    runMyMonadSE 42 myf1 >>= print
    runMyMonadSE 42 mySE_1 >>= print    
    runMyMonadSE 42 mySE_2 >>= print    
    runMyMonadES 42 myf1 >>= print    
    runMyMonadES 42 myES_1 >>= print    
    runMyMonadES 42 myES_2 >>= print    
