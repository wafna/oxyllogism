module Main where

import qualified Data.Map as Map
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
    x1 <- return $ p ∧ q
    putStrLn $ show x1
    putStrLn $ show $ evaluate x1 $ Map.fromList [("P", True), ("Q", False)]
    putStrLn $ show $ evaluate x1 $ Map.fromList [("P", True), ("Q", True)]
    x2 <- return $ p ∧ (neg q)
    putStrLn $ show $ evaluate x2 $ Map.fromList [("P", True), ("Q", False)]
    putStrLn $ show $ substitute x2 "Q" $ r ∨ q
    putStrLn $ show $ modusPonens (p ⊃ q) p 
    putStrLn $ show $ modusPonens (x2 ⊃ q) x2 
    putStrLn $ show $ modusPonens ((¬) x2 ∨ q) x2
