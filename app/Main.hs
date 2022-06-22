module Main where

import Data.Map

data Sentence 
    = Prop String
    | Not Sentence
    | And Sentence Sentence
    | Or Sentence Sentence
    deriving (Eq, Show, Read)

type Valuation = Map String Bool

evaluate :: Sentence -> Valuation -> Bool
evaluate s v = case s of
    Prop name -> v ! name
    Not p -> not $ evaluate p v
    And p q -> (evaluate p v) && (evaluate q v)
    Or p q -> (evaluate p v) || (evaluate q v)

main :: IO ()
main = 
    do
    putStrLn "Hello, Haskell!"
    s <- return $ And (Prop "P") (Prop "Q")
    putStrLn $ show s
    putStrLn $ show $ evaluate s $ fromList [("P", True), ("Q", False)]
