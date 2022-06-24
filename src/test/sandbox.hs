{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import Data.Functor.Identity

-- https://stackoverflow.com/questions/31838776/whats-the-difference-between-statet-s-exceptt-e-m-and-exceptt-e-statet-s-m

test1 :: (MonadState Int m, MonadError String m) => m Bool
test1 = do
  put 1
  throwError "foobar"
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

main :: IO ()
main = do
  print $ runIdentity . runStateExceptT 3 $ test1
  print $ runIdentity . runExceptStateT 3 $ test1
  print $ runIdentity . runStateExceptT 3 $ test2
  print $ runIdentity . runExceptStateT 3 $ test2
