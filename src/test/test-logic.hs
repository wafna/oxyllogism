module Main where

import qualified Data.Map as Map
import Data.List (intercalate)
import Test.Hspec
import Logic
    
pendingTests :: Spec
pendingTests = do
    it "needs some tests" pending

showValuation :: Valuation -> String
showValuation v = intercalate ", " (map showAssignment $ Map.toList v)
    where
    showAssignment (n, v') = n ++ "=" ++ show v'

evaluateSpec :: Spec
evaluateSpec = 
    let
        p = Prop "p"
        q = Prop "q"
    in
    describe "evaluate" $ do
        describe "const" $ do
            evaluate1 true True $ valuation []
            evaluate1 (neg true) False $ valuation []
            evaluate1 false False $ valuation []
            evaluate1 (neg false) True $ valuation []
        describe "∧" $ do
            eval <- return $ evaluate1 (p ∧ q) 
            eval False $ valuation [(propName p, False), (propName q, False)]
            eval False $ valuation [(propName p, True), (propName q, False)]
            eval False $ valuation [(propName p, False), (propName q, True)]
            eval True $ valuation [(propName p, True), (propName q, True)]
        describe "∨" $ do
            eval <- return $ evaluate1 (p ∨ q) 
            eval False $ valuation [(propName p, False), (propName q, False)]
            eval True $ valuation [(propName p, True), (propName q, False)]
            eval True $ valuation [(propName p, False), (propName q, True)]
            eval True $ valuation [(propName p, True), (propName q, True)]
        describe "⊃" $ do
            eval <- return $ evaluate1 (p ⊃ q) 
            eval True $ valuation [(propName p, False), (propName q, False)]
            eval False $ valuation [(propName p, True), (propName q, False)]
            eval True $ valuation [(propName p, False), (propName q, True)]
            eval True $ valuation [(propName p, True), (propName q, True)]
        describe "≡" $ do
            eval <- return $ evaluate1 (p ≡ q) 
            eval True $ valuation [(propName p, False), (propName q, False)]
            eval False $ valuation [(propName p, True), (propName q, False)]
            eval False $ valuation [(propName p, False), (propName q, True)]
            eval True $ valuation [(propName p, True), (propName q, True)]
        describe "≢" $ do
            eval <- return $ evaluate1 (p ≢ q) 
            eval False $ valuation [(propName p, False), (propName q, False)]
            eval True $ valuation [(propName p, True), (propName q, False)]
            eval True $ valuation [(propName p, False), (propName q, True)]
            eval False $ valuation [(propName p, True), (propName q, True)]
    where
    evaluate1 s e v = it (show s ++ " is " ++ show e ++ showWhere) $ e == evaluate s v
        where
        showWhere = if Map.null v then "" else " when " ++ showValuation v

axiomsSpec :: Spec
axiomsSpec = 
    let
        p = Prop "p"
        q = Prop "q"
        r = Prop "r"
    in
    describe "axioms" $ do
        pq <- return $ valueSet [propName p, propName q]
        pqr <- return $ valueSet [propName p, propName q, propName r]

        tautology pq $ p ⊃ (q ⊃ p)
        tautology pq $ ((neg p) ⊃ (neg q)) ⊃ (q ⊃ p)
        tautology pqr $ (p ⊃ (q ⊃ r)) ⊃ ((p ⊃ q) ⊃ (p ⊃ r))
    where
    tautology v t = describe (show t) $ sequence_ $ map (evaluate1 t True) $ map valuation v
    evaluate1 s e v = it (showValuation v) $ e == evaluate s v

substitutionSpec :: Spec
substitutionSpec = describe "substitutions" $ do
    pendingTests

main :: IO ()
main = hspec $ do
    evaluateSpec
    axiomsSpec
    substitutionSpec