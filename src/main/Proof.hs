module Proof where

import Control.Monad.State

import Logic

-- | A rule applied in a derivation.
data Rule
    = Premise Sentence
    | DoubleNegApply Int
    | DoubleNegRemove Int
    | SimplifyLeft Int
    | SimplifyRight Int
    | ModusPonens Int Int
    | ModusTollens Int Int
    | ModusTollendoPonens Int Int
    deriving (Eq, Show)

-- | Each step in the derivation is a rule and an expected result.
data Step = Step { stepRule :: Rule, stepResult :: Sentence }
    deriving (Eq, Show)

-- | A goal reached from a series of steps.
data Derivation = Derivation { derivationGoal :: Sentence, derivationSteps :: [Step] }
    deriving (Show)

type Derivator = State Derivation

-- | Starting with a goal, provide a series of steps to achieve the goal.
derive :: Sentence -> Derivator () -> Derivation
derive goal actor = execState actor $ Derivation goal [] 

currentStep :: Derivator Int
currentStep = gets $ \ (Derivation _ steps) -> length steps - 1

nthStep :: Int -> Derivator Step
nthStep nth = gets $ \ d -> (reverse $ derivationSteps d) !! nth

addStep :: Rule -> Sentence -> Derivator Int
addStep rule result = do
    modify $ \ d -> d { derivationSteps = (Step rule result) : derivationSteps d }
    currentStep

qed :: Derivator ()
qed = do
    r <- gets $ stepResult . head . derivationSteps
    g <- gets derivationGoal
    if r /= g
        then error "Goal not reached."
        else return ()

-- | Introduce a premise, return the step number.
pr :: Sentence -> Derivator Int
pr s = addStep (Premise s) s

-- | Apply modus ponens using the two steps, check the result, return step number.
-- The fist index must point to the conditional.
mp :: Int -> Int -> Sentence -> Derivator Int
mp p q r = do
    x <- nthStep p
    y <- nthStep q
    case modusPonens (stepResult x) (stepResult y) of
        Nothing -> error $ "Invalid application: " ++ show (stepResult x) ++ " mp " ++ show (stepResult y) ++ " -> " ++ show r
        Just u -> if (u /= r)
            then error "Wrong result."
            else addStep (ModusPonens p q) r

