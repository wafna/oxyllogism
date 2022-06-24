{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Control.Monad.State
import Control.Monad.Except
import Data.Functor.Identity

-- https://stackoverflow.com/questions/31838776/whats-the-difference-between-statet-s-exceptt-e-m-and-exceptt-e-statet-s-m

type MyMonad a = StateT Int (ExceptT String Identity) a

runMe :: Int -> MyMonad a -> Either String Int
runMe s q = fmap snd $ runIdentity $ runExceptT $ runStateT q s

doGood :: Int -> MyMonad Int
doGood i = put i >> return i

doBad :: String -> MyMonad ()
doBad s = throwError s

doSomething :: MyMonad Int
doSomething = get

main :: IO ()
main = do
    print $ runMe 42 $ do
        put 11
    print $ runMe 86 $ do
        throwError "biffed"
    print $ runMe 99 (doGood 86)
    print $ runMe 13 (doBad "wack")
    print $ runMe 303 doSomething
