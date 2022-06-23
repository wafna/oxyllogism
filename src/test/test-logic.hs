module Main where

import Logic
import qualified Data.Map as Map
import Data.List (intercalate)
import Test.Hspec
    
pendingTests :: Spec
pendingTests = do
    it "needs some tests" pending

substitutionSpec :: Spec
substitutionSpec = describe "substitutions" $ do
    pendingTests

evaluate1 :: Sentence -> Bool -> Valuation -> Spec
evaluate1 s e v = it (show s ++ showWhere ++ " is " ++ show e) $ e == evaluate s v
    where
    showWhere = if Map.null v then "" else ", where " ++ showValuation ++ ","
    showValuation = intercalate ", " (map showAssignment $ Map.toList v)
    showAssignment (n, v') = n ++ "=" ++ show v'

evaluateSpec :: Spec
evaluateSpec = 
    let
        p = Prop "P"
        q = Prop "Q"
        r = Prop "R"
    in
    describe "evaluate" $ do
        evaluate1 true True $ valuation []
        evaluate1 (neg true) False $ valuation []
        evaluate1 false False $ valuation []
        evaluate1 (neg false) True $ valuation []
        evaluate1 (p ∧ q) False $ valuation [(propName p, False), (propName q, False)]
        evaluate1 (p ∧ q) False $ valuation [(propName p, True), (propName q, False)]
        evaluate1 (p ∧ q) False $ valuation [(propName p, False), (propName q, True)]
        evaluate1 (p ∧ q) True $ valuation [(propName p, True), (propName q, True)]
        evaluate1 (p ∨ q) False $ valuation [(propName p, False), (propName q, False)]
        evaluate1 (p ∨ q) True $ valuation [(propName p, True), (propName q, False)]
        evaluate1 (p ∨ q) True $ valuation [(propName p, False), (propName q, True)]
        evaluate1 (p ∨ q) True $ valuation [(propName p, True), (propName q, True)]
        evaluate1 (p ⊃ q) True $ valuation [(propName p, False), (propName q, False)]
        evaluate1 (p ⊃ q) False $ valuation [(propName p, True), (propName q, False)]
        evaluate1 (p ⊃ q) True $ valuation [(propName p, False), (propName q, True)]
        evaluate1 (p ⊃ q) True $ valuation [(propName p, True), (propName q, True)]
        evaluate1 (p ≡ q) True $ valuation [(propName p, False), (propName q, False)]
        evaluate1 (p ≡ q) False $ valuation [(propName p, True), (propName q, False)]
        evaluate1 (p ≡ q) False $ valuation [(propName p, False), (propName q, True)]
        evaluate1 (p ≡ q) True $ valuation [(propName p, True), (propName q, True)]
        evaluate1 (p ≢ q) False $ valuation [(propName p, False), (propName q, False)]
        evaluate1 (p ≢ q) True $ valuation [(propName p, True), (propName q, False)]
        evaluate1 (p ≢ q) True $ valuation [(propName p, False), (propName q, True)]
        evaluate1 (p ≢ q) False $ valuation [(propName p, True), (propName q, True)]

tautologiesSpec :: Spec
tautologiesSpec = 
    let
        p = Prop "P"
        q = Prop "Q"
        r = Prop "R"
    in
    describe "tautologies" $ do
        pq <- return
            [ [(propName p, False), (propName q, False)]
            , [(propName p, True) , (propName q, False)]
            , [(propName p, False), (propName q, True)]
            , [(propName p, True) , (propName q, True)]
            ]

        pqr <- return
            [ [(propName p, False), (propName q, False), (propName r, False)]
            , [(propName p, True) , (propName q, False), (propName r, False)]
            , [(propName p, False), (propName q, True) , (propName r, False)]
            , [(propName p, True) , (propName q, True) , (propName r, False)]
            , [(propName p, False), (propName q, False), (propName r, True)]
            , [(propName p, True) , (propName q, False), (propName r, True)]
            , [(propName p, False), (propName q, True) , (propName r, True)]
            , [(propName p, True) , (propName q, True) , (propName r, True)]
            ]

        tautology pq $ p ⊃ (q ⊃ p)
        tautology pq $ ((neg p) ⊃ (neg q)) ⊃ (q ⊃ p)
        tautology pqr $ (p ⊃ (q  ⊃ r)) ⊃ ((p ⊃ q) ⊃ (p ⊃ r))
    where
    tautology v t = sequence_ $ map (evaluate1 t True) $ map valuation v


main :: IO ()
main = hspec $ do
    evaluateSpec
    tautologiesSpec
    substitutionSpec