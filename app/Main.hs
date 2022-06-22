module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Logic


main :: IO ()
main = 
    let
        p = Prop "P"
        q = Prop "Q"
        r = Prop "R"
    in
    do
    putStrLn "Hello, Logic!"
    x <- return $ p ∧ q
    putStrLn $ show x
    putStrLn $ show $ evaluate x $ Map.fromList [("P", True), ("Q", False)]
    putStrLn $ show $ evaluate x $ Map.fromList [("P", True), ("Q", True)]
    x <- return $ p ∧ (neg q)
    putStrLn $ show $ evaluate x $ Map.fromList [("P", True), ("Q", False)]
    putStrLn $ show $ substitute x "Q" $ r ∨ q
    putStrLn $ show $ modusPonens (p ⊃ q) p 
    putStrLn $ show $ modusPonens (x ⊃ q) x 
    putStrLn $ show $ modusPonens ((¬) x ∨ q) x 