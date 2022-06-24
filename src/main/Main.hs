-- | Sandbox for logic.
module Main (main) where

import qualified Data.Map as Map

import Logic
import Proof

-- | Have at it...
main :: IO ()
main = 
    let
        p = Prop "p"
        q = Prop "q"
        r = Prop "r"

        hr = putStrLn $ take 50 $ repeat '-'
    in
    do
    putStrLn "Oxyllogism"

    putStrLn "Evaluation..."
    x1 <- return $ p ∧ q
    putStrLn $ show x1
    putStrLn $ show $ evaluate x1 $ Map.fromList [(propName p, True), (propName q, False)]
    x2 <- return $ p ∧ (neg q)
    putStrLn $ show $ evaluate x2 $ Map.fromList [(propName p, True), (propName q, False)]

    putStrLn "Substitution..."
    putStrLn $ show $ substitute x2 "Q" $ r ∨ q

    putStrLn "Modus ponens..."
    putStrLn $ show $ modusPonens (p ⊃ q) p
    putStrLn $ show $ modusPonens (x1 ⊃ x2) x1 
    putStrLn $ show $ modusPonens (x2 ⊃ x1) x1
    -- If you use the negation operator you must section it.
    putStrLn $ show $ modusPonens ((¬)x2 ∨ q) x2

    putStrLn "Derivation..."
    hr
    putStrLn $ showDerivation $ derive (neg r) $ 
        do
        i1 <- pr $ p ⊃ (q ⊃ (neg r))
        i2 <- pr p
        i3 <- pr q
        i4 <- mp i1 i2 $ q ⊃ (neg r)
        i5 <- mp i4 i3 $ neg r
        qed i5
    hr
    putStrLn $ showDerivation $ derive q $
        do
        i1 <- pr $ p ⊃ ((neg q) ⊃ r)
        i2 <- pr $ neg p ⊃ r
        i3 <- pr $ neg r
        i4 <- mt i2 i3 $ (neg (neg p))
        i5 <- dnr i4 p
        i6 <- mp i1 i5 $ neg q ⊃ r
        i7 <- mt i6 i3 $ (neg (neg q))
        i8 <- dnr i7 q
        qed i8
