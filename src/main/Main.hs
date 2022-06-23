-- | Sandbox for logic.
module Main where

import qualified Data.Map as Map
import Logic

-- | Have at it...
main :: IO ()
main = 
    let
        p = Prop "P"
        q = Prop "Q"
        r = Prop "R"
    in
    do
    putStrLn "Oxyllogism"
    x1 <- return $ p ∧ q
    putStrLn $ show x1
    putStrLn $ show $ evaluate x1 $ Map.fromList [("P", True), ("Q", False)]
    x2 <- return $ p ∧ (neg q)
    putStrLn $ show $ evaluate x2 $ Map.fromList [("P", True), ("Q", False)]
    putStrLn $ show $ substitute x2 "Q" $ r ∨ q
    putStrLn $ show $ modusPonens (p ⊃ q) p
    putStrLn $ show $ modusPonens (x1 ⊃ x2) x1 
    putStrLn $ show $ modusPonens (x2 ⊃ x1) x1 
    -- If you use the negation operator you must section it.
    putStrLn $ show $ modusPonens ((¬) x2 ∨ q) x2
