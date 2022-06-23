-- | Sandbox for logic.
module Main where

import qualified Data.Map as Map

import Logic
import Proof

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


    putStrLn "Evaluation..."
    x1 <- return $ p ∧ q
    putStrLn $ show x1
    putStrLn $ show $ evaluate x1 $ Map.fromList [("P", True), ("Q", False)]
    x2 <- return $ p ∧ (neg q)
    putStrLn $ show $ evaluate x2 $ Map.fromList [("P", True), ("Q", False)]

    putStrLn "Substitution..."
    putStrLn $ show $ substitute x2 "Q" $ r ∨ q

    putStrLn "Modus ponens..."
    putStrLn $ show $ modusPonens (p ⊃ q) p
    putStrLn $ show $ modusPonens (x1 ⊃ x2) x1 
    putStrLn $ show $ modusPonens (x2 ⊃ x1) x1
    -- If you use the negation operator you must section it.
    putStrLn $ show $ modusPonens ((¬) x2 ∨ q) x2

    putStrLn $ show $ modusPonens (p ⊃ (q ⊃ (¬)r)) p

    putStrLn "Derivation..."
    putStrLn $ show $ derive (neg r) $ do
        i1 <- pr $ p ⊃ (q ⊃ (neg r))
        i2 <- pr p
        i3 <- pr q
        i4 <- mp i1 i2 $ q ⊃ (neg r)
        mp i4 i3 (neg r)
        qed
