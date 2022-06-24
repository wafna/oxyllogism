{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

-- import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import Data.Functor.Identity

-- https://stackoverflow.com/questions/31838776/whats-the-difference-between-statet-s-exceptt-e-m-and-exceptt-e-statet-s-m

{-
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


-- runMyMonad :: Int -> Either String Int


f0 :: Either String Int
f0 = fmap snd $ runIdentity . runExceptStateT 3 $ test2

f1 :: Either String Int
f1 = fmap snd $ runIdentity (runExceptStateT 3 test2)  
-}

type MyMonad a = StateT Int (ExceptT String Identity) a

runMe :: Int -> MyMonad a -> Either String Int
runMe s q = fmap snd $ runIdentity $ runExceptT $ runStateT q s

doGood :: Int -> MyMonad ()
doGood i = put i

doBad :: String -> MyMonad ()
doBad s = throwError s

doSomething :: MyMonad Int
doSomething = get

main :: IO ()
main = do
    -- print $ runIdentity . runStateExceptT 3 $ test1
    -- print $ runIdentity . runExceptStateT 3 $ test1
    -- print $ runIdentity . runStateExceptT 3 $ test2
    -- print $ runIdentity . runExceptStateT 3 $ test2
    -- print $ fmap snd $ runIdentity . runExceptStateT 3 $ test2
    -- print f0
    -- print f1
    print $ runMe 42 $ do
        put 11
    print $ runMe 86 $ do
        throwError "fooked"
    print $ runMe 99 (doGood 86)
    print $ runMe 13 (doBad "fooked")
    print $ runMe 303 doSomething
