module Logic where 

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Sentence 
    = Const Bool
    | Prop { propName :: String }
    | Not Sentence
    | And Sentence Sentence
    | Or Sentence Sentence
    | If Sentence Sentence
    | Iff Sentence Sentence
    | Xor Sentence Sentence
    deriving (Eq, Read)

instance Show Sentence where
    show s = case s of 
        Const b -> if b then "t" else "f"
        Prop n -> n
        Not p -> "¬" ++ show p
        And p q -> "(" ++ show p ++ " ∧ " ++ show q ++ ")"
        Or p q -> "(" ++ show p ++ " ∨ " ++ show q ++ ")"
        If p q -> "(" ++ show p ++ " ⊃ " ++ show q ++ ")"
        Iff p q -> "(" ++ show p ++ " ≡ " ++ show q ++ ")"
        Xor p q -> "(" ++ show p ++ " ≢ " ++ show q ++ ")"

true :: Sentence
true = Const True
false :: Sentence
false = Const False
-- Can't use `not` or `negate`.
neg :: Sentence -> Sentence
neg = Not
(¬) :: Sentence -> Sentence
(¬) = Not
(∧) :: Sentence -> Sentence -> Sentence
(∧) = And
(∨) :: Sentence -> Sentence -> Sentence
(∨) = Or
(⊃) :: Sentence -> Sentence -> Sentence
(⊃) = If
(≡) :: Sentence -> Sentence -> Sentence
(≡) = Iff
(≢) :: Sentence -> Sentence -> Sentence
(≢) = Xor

-- Analagous to arithmetic.
infixl 8  ≡
infixl 7  ∧
infixl 6  ∨, ⊃

type Valuation = Map String Bool

valuation :: [(String, Bool)] -> Map String Bool
valuation vs = Map.fromList vs

-- valueSet :: [String] -> [(String, Bool)]
-- valueSet names = builder (2 ** length names) $ (flip map) names $ \ name -> (name, False)
--     where
--     builder n perms = if n == 0 
--         then perms
--         else (flip map) (head perms) $ \ (n, v) -> (n)

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
    Xor p q -> 
        let p' = evaluate p v
            q' = evaluate q v
        in
        (not p' && q') || (p' && not q')

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
    Xor p q -> both p q
    where
    both p q = (atoms p) `Set.union` (atoms q)

substitute :: Sentence -> String -> Sentence -> Either String Sentence
substitute source target substitution =
    let 
        -- It's ok if the target is part of the substitution.
        overlap = (Set.delete target $ atoms source) `Set.intersection` (atoms substitution)
        subs src = case src of
            c@(Const _) -> c
            Prop name -> if name == target
                then substitution
                else src
            Not p -> Not $ subs p
            And p q -> (subs p) ∧ (subs q)
            Or p q -> (subs p) ∨ (subs q)
            If p q -> (subs p) ⊃ (subs q)
            Iff p q -> (subs p) ≡ (subs q)
            Xor p q -> (subs p) ≢ (subs q)
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
    _ -> Nothing


addition :: Sentence -> Sentence -> Sentence
addition p q = p ∨ q

simplification :: Sentence -> Sentence -> Maybe Sentence
simplification conjunction element = case conjunction of 
    Or p q -> if (p == element) then Just q else if (q == element) then Just p else Nothing
    _ -> Nothing

adjunction :: Sentence -> Sentence -> Sentence
adjunction p q = p ∧ q

invert :: Sentence -> Sentence
invert s = case s of
    p@(Prop _) -> Not p
    Const b -> Const $ not b
    Not p -> p
    Or p q -> And (neg p) (neg q)
    And p q -> Or (neg p) (neg q)
    If p q -> And p (neg q)
    Iff p q -> Xor p q
    Xor p q -> Iff p q
