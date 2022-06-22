module Logic where 

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Sentence 
    = Const Bool
    | Prop String
    | Not Sentence
    | And Sentence Sentence
    | Or Sentence Sentence
    | If Sentence Sentence
    | Iff Sentence Sentence
    deriving (Eq, Show, Read)

-- Can't use `not` or `negate`.
neg :: Sentence -> Sentence
neg = Not

(¬) = Not
(∧) = And
(∨) = Or
(⊃) = If
(≡) = Iff

-- Analagous to arithmetic.
infixl 8  ≡
infixl 7  ∧
infixl 6  ∨, ⊃

type Valuation = Map String Bool

-- Evaluate a sentence with a given valuation of its atoms.
-- Errors if the valuation doesn't satisfy all atoms.
evaluate :: Sentence -> Valuation -> Bool
evaluate s v = case s of
    Const b -> b
    Prop name -> v Map.! name
    Not p -> not $ evaluate p v
    And p q -> (evaluate p v) && (evaluate q v)
    Or p q -> (evaluate p v) || (evaluate q v)
    If p q -> (not $ evaluate p v) || (evaluate q v)
    Iff p q -> 
        let p' = evaluate p v
            q' = evaluate q v
        in
        (p' && q') || (not p' && not q')

-- Find all atoms in a sentence.
atoms :: Sentence -> Set String
atoms s = case s of
    Const _ -> Set.empty
    Prop name -> Set.fromList [name]
    Not p -> atoms p
    And p q -> both p q
    Or p q -> both p q
    If p q -> both p q
    Iff p q -> both p q
    where
    both p q = (atoms p) `Set.union` (atoms q)

substitute :: Sentence -> String -> Sentence -> Either String Sentence
substitute source target substitution =
    let 
        -- It's ok if the target is part of the substitution.
        overlap = (Set.delete target $ atoms source) `Set.intersection` (atoms substitution)
        subs source = case source of
            c@(Const _) -> c
            Prop name -> if name == target
                then substitution
                else source
            Not p -> Not $ subs p
            And p q -> (subs p) ∧ (subs q)
            Or p q -> (subs p) ∨ (subs q)
            If p q -> (subs p) ⊃ (subs q)
            Iff p q -> (subs p) ≡ (subs q)
    in
    if Set.null overlap
        then Right $ subs source
        else Left $ "Substitution clashes with target on " ++ show overlap

modusPonens :: Sentence -> Sentence -> Maybe Sentence
modusPonens conditional antecedent = case conditional of
    If p q -> if (p == antecedent) then Just q else Nothing
    Or p q -> if (neg antecedent == p) then Just q else Nothing
    _ -> Nothing

modusTollens :: Sentence -> Sentence -> Maybe Sentence
modusTollens conditional consequent = case conditional of
    If (Not p) (Not q) -> if (q == consequent) then Just p else Nothing
    If p q -> if (q == neg consequent) then Just $ neg p else Nothing
    Or (Not p) q -> if (q == neg consequent) then Just p else Nothing
    Or p (Not q) -> if (q == consequent) then Just p else Nothing
    _ -> Nothing

modusTollensPonens :: Sentence -> Sentence -> Maybe Sentence
modusTollensPonens disjunction denial = case denial of
    Not p -> case disjunction of
        Or x y -> if (p == x) then Just y else if (p == y) then Just x else Nothing
        _ -> Nothing


addition :: Sentence -> Sentence -> Sentence
addition p q = p ∨ q

simplification :: Sentence -> Sentence -> Maybe Sentence
simplification conjunction element = case conjunction of 
    Or p q -> if (p == element) then Just q else if (q == element) then Just p else Nothing
    _ -> Nothing

adjunction :: Sentence -> Sentence -> Sentence
adjunction p q = p ∧ q
