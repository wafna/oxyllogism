module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Sentence 
    = Prop String
    | Not Sentence
    | And Sentence Sentence
    | Or Sentence Sentence
    | If Sentence Sentence
    deriving (Eq, Show, Read)

type Valuation = Map String Bool

-- Evaluate a sentence with a given valuation of its atoms.
-- Errors if the valuation doesn't satisfy all atoms.
evaluate :: Sentence -> Valuation -> Bool
evaluate s v = case s of
    Prop name -> v Map.! name
    Not p -> not $ evaluate p v
    And p q -> (evaluate p v) && (evaluate q v)
    Or p q -> (evaluate p v) || (evaluate q v)
    If p q -> (not $ evaluate p v) || (evaluate q v)

-- Find all atoms in a sentence.
atoms :: Sentence -> Set String
atoms s = case s of
    Prop name -> Set.fromList [name]
    Not p -> atoms p
    And p q -> (atoms p) `Set.union` (atoms q)
    Or p q -> (atoms p) `Set.union` (atoms q)
    If p q -> (atoms p) `Set.union` (atoms q)

-- Can't use `not` or `negate`.
neg :: Sentence -> Sentence
neg = Not

(/&) :: Sentence -> Sentence -> Sentence
(/&) = And

(/|) :: Sentence -> Sentence -> Sentence
(/|) = Or

(/>) :: Sentence -> Sentence -> Sentence
(/>) = If

-- Analagous to arithmetic.
infixl 7  /&
infixl 6  /|, />

main :: IO ()
main = 
    let
        p = Prop "P"
        q = Prop "Q"
    in
    do
    putStrLn "Hello, Haskell!"
    s <- return $ And p q
    putStrLn $ show s
    putStrLn $ show $ evaluate s $ Map.fromList [("P", True), ("Q", False)]
    putStrLn $ show $ evaluate s $ Map.fromList [("P", True), ("Q", True)]
    s <- return $ p /& (neg q)
    putStrLn $ show $ evaluate s $ Map.fromList [("P", True), ("Q", False)]
