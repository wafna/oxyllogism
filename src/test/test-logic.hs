module Main where

import qualified Data.Map as Map
import qualified Data.Either as Either
import Data.List (intercalate)
import Test.Hspec
import Logic

p :: Sentence    
p = Prop "p"
q :: Sentence
q = Prop "q"
r :: Sentence
r = Prop "r"

showBool :: Bool -> String
showBool b = if b then "T" else "F"

showValuation :: Valuation -> String
showValuation v = intercalate ", " (map showAssignment $ Map.toList v)
    where
    showAssignment (n, v') = concat [n, " = ", showBool v']

evaluateSpec :: Spec
evaluateSpec = 
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
        describe "↑" $ do
            eval <- return $ evaluate1 (p ↑ q) 
            eval True $ valuation [(propName p, False), (propName q, False)]
            eval True $ valuation [(propName p, True), (propName q, False)]
            eval True $ valuation [(propName p, False), (propName q, True)]
            eval False $ valuation [(propName p, True), (propName q, True)]
        describe "∨" $ do
            eval <- return $ evaluate1 (p ∨ q) 
            eval False $ valuation [(propName p, False), (propName q, False)]
            eval True $ valuation [(propName p, True), (propName q, False)]
            eval True $ valuation [(propName p, False), (propName q, True)]
            eval True $ valuation [(propName p, True), (propName q, True)]
        describe "↓" $ do
            eval <- return $ evaluate1 (p ↓ q) 
            eval True $ valuation [(propName p, False), (propName q, False)]
            eval False $ valuation [(propName p, True), (propName q, False)]
            eval False $ valuation [(propName p, False), (propName q, True)]
            eval False $ valuation [(propName p, True), (propName q, True)]
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
        describe "⊕" $ do
            eval <- return $ evaluate1 (p ⊕ q) 
            eval False $ valuation [(propName p, False), (propName q, False)]
            eval True $ valuation [(propName p, True), (propName q, False)]
            eval True $ valuation [(propName p, False), (propName q, True)]
            eval False $ valuation [(propName p, True), (propName q, True)]
    where
    evaluate1 s e v = it (concat [show s, " is ", showBool e, showWhere]) $ e == evaluate s v
        where
        showWhere = if Map.null v then "" else " when " ++ showValuation v

axiomsSpec :: Spec
axiomsSpec = 
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
substitutionSpec = 
    describe "substitutions" $ do
        success (p ∧ (neg q)) (propName q) r (p ∧ (neg r))
        success (p ∧ (neg q)) (propName q) (q ⊃ r) (p ∧ (neg (q ⊃ r)))
        failure (p ∧ (neg q)) (propName q) p
    where
    success src target sub res = it (concat ["sub ", show sub, " for ", target, " in ", show src, " yields ", show res]) $ 
        Either.either (const False) ((==) res) $ substitute src target sub
    failure src target sub = it (concat ["sub ", show sub, " for ", target, " in ", show src, " fails."]) $
        Either.isLeft $ substitute src target sub

transformationsSpec :: Spec
transformationsSpec = describe "transformations" $ do
    describe "invert" $ do
        invert1 true false
        invert1 false true
        invert1 p $ neg p
        invert1 (neg p) $ p
        invert1 (p ∧ q) $ (neg p ∨ neg q)
        invert1 (p ∨ q) $ (neg p ∧ neg q)
        invert1 (p ≡ q) $ (p ⊕ q)
        invert1 (p ⊕ q) $ (p ≡ q)
    where
    invert1 x y = it (show x ++ " yields " ++ show y) $ y == invert x

inferencesSpec :: Spec
inferencesSpec = it "inferences" pending

main :: IO ()
main = hspec $ do
    evaluateSpec
    axiomsSpec
    substitutionSpec
    transformationsSpec