-- | Construct, transform, and derive sentences (formulae) in formal logic.
module Logic where 

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- | Structure of sentences in propositional logic.
data Sentence 
    = Const Bool
    | Prop { propName :: String }
    | Not Sentence
    | And Sentence Sentence
    | Nand Sentence Sentence
    | Or Sentence Sentence
    | Nor Sentence Sentence
    | If Sentence Sentence
    | Iff Sentence Sentence
    | Xor Sentence Sentence
    deriving (Eq)

-- | Pretty it up.
instance Show Sentence where
    show s = case s of 
        Const b -> if b then "t" else "f"
        Prop n -> n
        Not p -> "¬" ++ show p
        And p q -> "(" ++ show p ++ " ∧ " ++ show q ++ ")"
        Nand p q -> "(" ++ show p ++ " ↑ " ++ show q ++ ")"
        Or p q -> "(" ++ show p ++ " ∨ " ++ show q ++ ")"
        Nor p q -> "(" ++ show p ++ " ↓ " ++ show q ++ ")"
        If p q -> "(" ++ show p ++ " ⊃ " ++ show q ++ ")"
        Iff p q -> "(" ++ show p ++ " ≡ " ++ show q ++ ")"
        Xor p q -> "(" ++ show p ++ " ⊕ " ++ show q ++ ")"

-- Operators and constants.
-- Functions are provided for convenient infix notation without special unicode.

-- | True
true :: Sentence
true = Const True
-- | False
false :: Sentence
false = Const False
-- | Negation
neg :: Sentence -> Sentence
neg = Not
-- | Negation
(¬) :: Sentence -> Sentence
(¬) = Not
-- | Conjunction
and :: Sentence -> Sentence -> Sentence
and = And
-- | Conjunction
(∧) :: Sentence -> Sentence -> Sentence
(∧) = And
-- | Negation of conjunction
(↑) :: Sentence -> Sentence -> Sentence
(↑) = Nand
-- | Negation of conjunction
nand :: Sentence -> Sentence -> Sentence
nand = Nand
-- | Disjunction
(∨) :: Sentence -> Sentence -> Sentence
(∨) = Or
-- | Disjunction
or :: Sentence -> Sentence -> Sentence
or = Or
-- | Negation of disjunction
(↓) :: Sentence -> Sentence -> Sentence
(↓) = Nor
-- | Negation of disjunction
nor :: Sentence -> Sentence -> Sentence
nor = Nor
-- | Material implication
(⊃) :: Sentence -> Sentence -> Sentence
(⊃) = If
-- | Material implication
implies :: Sentence -> Sentence -> Sentence
implies = If
-- | Material equivalence
(≡) :: Sentence -> Sentence -> Sentence
(≡) = Iff
-- | Material equivalence
iff :: Sentence -> Sentence -> Sentence
iff = Iff
-- | Exclusive disjunction.
(⊕) :: Sentence -> Sentence -> Sentence
(⊕) = Xor
-- | Exclusive disjunction.
xor :: Sentence -> Sentence -> Sentence
xor = Xor

-- Analagous to arithmetic.
infixl 8  ≡, ⊕, `iff`, `xor`
infixl 7  ∧, ↑, `nand`
infixl 6  ∨, ⊃, ↓, `or`, `nor`, `implies`

-- | Pairings of names and truth values to be applied to a sentence.
type Valuation = Map String Bool

-- | To avoid importing `Map.fromList`.
valuation :: [(String, Bool)] -> Map String Bool
valuation vs = Map.fromList vs

-- | Given a list of names, produce all distinct permutations of truth value assignments to those names
valueSet :: [String] -> [[(String, Bool)]]
valueSet names = map (zip names) $ combos (length names) [True, False]
    where
    combos n r = case n of 
        0 -> [[]]
        _ -> [i:s | i <- r, s <- combos (n-1) r]


-- | Evaluate a sentence with a given valuation of its atoms.
-- Errors if the valuation doesn't satisfy all atoms.
evaluate :: Sentence -> Valuation -> Bool
evaluate s v = case s of
    Const b -> b
    Prop name -> v Map.! name
    Not p -> not $ evaluate p v
    And p q -> (evaluate p v) && (evaluate q v)
    Nand p q -> not $ (evaluate p v) && (evaluate q v)
    Or p q -> (evaluate p v) || (evaluate q v)
    Nor p q -> not $ (evaluate p v) || (evaluate q v)
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

-- |Find all atomic propositions in a sentence.
atoms :: Sentence -> Set String
atoms s = case s of
    Const _ -> Set.empty
    Prop name -> Set.fromList [name]
    Not p -> atoms p
    And p q -> both p q
    Nand p q -> both p q
    Or p q -> both p q
    Nor p q -> both p q
    If p q -> both p q
    Iff p q -> both p q
    Xor p q -> both p q
    where
    both p q = (atoms p) `Set.union` (atoms q)

-- Rules of inference.

-- | Substitute a sentence for a variable in another sentence.
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
            And p q -> And (subs p) (subs q)
            Nand p q -> Nand (subs p) (subs q)
            Or p q -> Or (subs p) (subs q)
            Nor p q -> Nor (subs p) (subs q)
            If p q -> If (subs p) (subs q)
            Iff p q -> Iff (subs p) (subs q)
            Xor p q -> Xor (subs p) (subs q)
    in
    if Set.null overlap
        then Right $ subs source
        else Left $ "Substitution clashes with target on " ++ show overlap

-- | Infer the consequent of a conditional from the antecedent.
modusPonens :: Sentence -> Sentence -> Maybe Sentence
modusPonens conditional antecedent = case conditional of
    If p q -> if (p == antecedent) then Just q else Nothing
    _ -> Nothing

-- | Infer the antecedant of a conditional from the denial of the consequent.
modusTollens :: Sentence -> Sentence -> Maybe Sentence
modusTollens conditional consequent = case conditional of
    If p q -> if (q == neg consequent) || (neg q == consequent) 
        then Just $ neg p 
        else Nothing
    _ -> Nothing

-- | Infer either side of a disjunction from the negation of either of its arguments.
modusTollendoPonens :: Sentence -> Sentence -> Maybe Sentence
modusTollendoPonens disjunction denial = case denial of
    Not p -> case disjunction of
        Or x y -> if (p == x) then Just y else if (p == y) then Just x else Nothing
        _ -> Nothing
    _ -> Nothing

-- | Infer either side of a conjunction from the other of its arguments.
simplification :: Sentence -> Sentence -> Maybe Sentence
simplification conjunction element = case conjunction of 
    Or p q -> if (p == element) then Just q else if (q == element) then Just p else Nothing
    _ -> Nothing

-- | Disjoin two sentences.
addition :: Sentence -> Sentence -> Sentence
addition p q = Or p q

-- | Conjoin two sentences.
adjunction :: Sentence -> Sentence -> Sentence
adjunction p q = And p q

-- | Infer a sentence from its double negation.
doubleNegation :: Sentence -> Maybe Sentence
doubleNegation p = case p of 
    Not (Not q) -> Just q
    _ -> Nothing

-- Transformations

-- | Negates a sentence by flipping the outermost connective where it can.
invert :: Sentence -> Sentence
invert s = case s of
    Prop _ -> Not s
    Const b -> Const $ not b
    Not p -> p
    And p q -> Or (neg p) (neg q)
    Nand p q -> And p q
    Or p q -> And (neg p) (neg q)
    Nor p q -> Or p q
    -- Ifs are not changed due to the way we will want to apply rules of inference.
    If _ _ -> Not $ s
    Iff p q -> Xor p q
    Xor p q -> Iff p q

