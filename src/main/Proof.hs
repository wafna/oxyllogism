module Proof (
    Derivator, Derivation, showDerivation,
    derive, qed, pr, dna, dnr, mp, mt,
    sl, sr, mtpl, mtpr, adj, add
) where

import qualified Data.List as List
import Data.Functor.Identity
import Control.Monad.State
import Control.Monad.Except

import Logic

-- | A rule applied in a derivation.
data Rule
    = Premise Sentence
    | DoubleNegApply Int
    | DoubleNegRemove Int
    | SimplifyLeft Int Int
    | SimplifyRight Int Int
    | ModusPonens Int Int
    | ModusTollens Int Int
    | ModusTollendoPonensLeft Int Int
    | ModusTollendoPonensRight Int Int
    | Adjunction Int Int
    | Addition Int Sentence
    deriving (Eq, Show)

-- | Each step in the derivation is a rule and an expected result.
data Step = Step { stepRule :: Rule, stepResult :: Sentence }
    deriving (Eq, Show)

-- | A goal reached from a series of steps.
data Derivation = Derivation { derivationGoal :: Sentence, derivationSteps :: [Step] }
    deriving (Show)

-- Derivation state monad.
-- type Derivator = State Derivation
type Derivator a = StateT Derivation (ExceptT String Identity) a

-- | Starting with a goal, provide a series of steps to achieve the goal.
derive :: Sentence -> Derivator () -> Either String Derivation
derive goal action = fmap snd $ runIdentity $ runExceptT $ runStateT action $ Derivation goal []

nthStep :: Int -> Derivator Step
nthStep nth = gets $ \ d -> (reverse $ derivationSteps d) !! (nth - 1)

addStep :: Rule -> Sentence -> Derivator Int
addStep rule result = do
    modify $ \ d -> d { derivationSteps = (Step rule result) : derivationSteps d }
    currentStep

currentStep :: Derivator Int
currentStep = gets $ \ (Derivation _ steps) -> length steps

-- | asserts that the result of the given step satisfies the goal of the derivation.
qed :: Int -> Derivator ()
qed i = do
    r <- nthStep i
    g <- gets derivationGoal
    if stepResult r /= g
        then throwError $ concat ["Goal not reached: found ", show r, " need ", show g]
        else return ()

checkResult :: Sentence -> Sentence -> Rule -> Derivator Int
checkResult expected actual rule =
    if expected /= actual
        then throwError $ concat ["Wrong result. Expected ", show expected, ", got ", show actual]
        else addStep rule actual

checkMaybeResult :: Sentence -> Maybe Sentence -> Rule -> Derivator Int
checkMaybeResult expected actual rule = 
    case actual of
        Nothing -> 
            currentStep >>= \ step -> 
                throwError $ concat ["Invalid application on step ", show $ 1 + step, ": expected ", show expected]
        Just u -> checkResult expected u rule

-- Rules

-- | Introduce a premise, return the step number.
pr :: Sentence -> Derivator Int
pr s = addStep (Premise s) s

-- | Apply double negation, return the step number.
dna :: Int -> Sentence -> Derivator Int
dna p r = nthStep p >>= \x -> checkResult r (neg $ neg $ stepResult x) (DoubleNegApply p) 

-- | Remove double negation, return the step number.
dnr :: Int -> Sentence -> Derivator Int
dnr p r = nthStep p >>= \ x -> checkMaybeResult r (doubleNegation $ stepResult x) (DoubleNegRemove p)

-- | Apply modus ponens using the two steps, check the result, return the step number.
-- The fist index must point to the conditional.
mp :: Int -> Int -> Sentence -> Derivator Int
mp p q r = do
    x <- nthStep p
    y <- nthStep q
    checkMaybeResult r (modusPonens (stepResult x) (stepResult y)) (ModusPonens p q)

-- | Apply modus tollens using the two steps, check the result, return the step number.
-- The fist index must point to the conditional.
mt :: Int -> Int -> Sentence -> Derivator Int
mt p q r = do
    x <- nthStep p
    y <- nthStep q
    checkMaybeResult r (modusTollens (stepResult x) (stepResult y)) (ModusTollens p q)

-- | Simplify left, return the step number.
sl :: Int -> Int -> Sentence -> Derivator Int
sl p q r = do
    x <- nthStep p
    y <- nthStep q
    checkMaybeResult r (simplifyLeft (stepResult x) (stepResult y)) (SimplifyLeft p q)

-- | Simplify right, return the step number.
sr :: Int -> Int -> Sentence -> Derivator Int
sr p q r = do
    x <- nthStep p
    y <- nthStep q
    checkMaybeResult r (simplifyRight (stepResult x) (stepResult y)) (SimplifyLeft p q)

-- | Modus tollendo ponens left, return the step number.
mtpl :: Int -> Int -> Sentence -> Derivator Int
mtpl p q r = do
    x <- nthStep p
    y <- nthStep q
    checkMaybeResult r (modusTollendoPonensLeft (stepResult x) (stepResult y)) (ModusTollendoPonensLeft p q)

-- | Modus tollendo ponens right, return the step number.
mtpr :: Int -> Int -> Sentence -> Derivator Int
mtpr p q r = do
    x <- nthStep p
    y <- nthStep q
    checkMaybeResult r (modusTollendoPonensRight (stepResult x) (stepResult y)) (ModusTollendoPonensRight p q)

-- | Conjoin two steps, return the step number.
adj :: Int -> Int -> Sentence -> Derivator Int
adj p q r = do
    x <- nthStep p
    y <- nthStep q
    checkResult r (adjunction  (stepResult x) (stepResult y)) (Adjunction p q)

-- | Disjoin any proposition, return the step number.
add :: Int -> Sentence -> Sentence -> Derivator Int
add p q r = do
    x <- nthStep p
    checkResult r (addition (stepResult x) q) (Addition p q)

-- Display.

spacedWords :: [String] -> String
spacedWords ws = List.intercalate " " ws

-- | Pretty it up.
showDerivation :: Derivation -> String
showDerivation d = List.intercalate "\n" $ map (\ (nth, Step rule result) -> concat [show nth, ".  ", showRule rule, show result]) $ zip [(1::Int)..] $ reverse $ derivationSteps d

showRule :: Rule -> String
showRule rule = case rule of
    Premise _ -> pad 20 "pr"
    DoubleNegApply p -> pad 20 $ spacedWords [pad 8 "dn(a)", show (1 + p)]
    DoubleNegRemove p -> pad 20 $ spacedWords [pad 8 "dn(r)", show (1 + p)]
    SimplifyLeft p q -> pad 20 $ spacedWords [pad 8 "s(l)", show (1 + p), show (1 + q)]
    SimplifyRight p q -> pad 20 $ spacedWords [pad 8 "s(r)", show (1 + p), show (1 + q)]
    ModusPonens p q -> pad 20 $ spacedWords [pad 8 "mp", show (1 + p), show (1 + q)]
    ModusTollens p q -> pad 20 $ spacedWords [pad 8 "mt", show (1 + p), show (1 + q)]
    ModusTollendoPonensLeft p q -> pad 20 $ spacedWords [pad 8 "mtp(l)", show (1 + p), show (1 + q)]
    ModusTollendoPonensRight p q -> pad 20 $ spacedWords [pad 8 "mtp(r)", show (1 + p), show (1 + q)]
    Adjunction p q -> pad 20 $ spacedWords [pad 8 "adj", show (1 + p), show (1 + q)]
    Addition p q -> pad 20 $ spacedWords [pad 8 "mtp(l)", show (1 + p), show q]

pad :: Int -> String -> String
pad n s = 
    let len = length s in
    if (len < n) 
        then s ++ concat (take (n - len) $ repeat " ") 
        else s

