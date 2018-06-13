{-# OPTIONS_GHC -Wall #-}

import Data.Foldable

main :: IO ()
main = do
  let terms =
        [I, I] :
        [K, A, B] :
        [S, Group [K, Group [S, I]], K, A, B] :
        -- [S, I, I, Group [S, I, I]] :
        []
  forM_ terms $ \ term -> do
    print term
    putStrLn "==>"
    print $ reduce term
    putStrLn "======================"

data Element = S | K | I | A | B | Group AST
  deriving (Show)
type AST = [Element]

reduce :: AST -> AST
reduce term = case term of
  -- I pattern matching
  I : a : rest -> reduce (a : rest)
  [I] -> [I]
  -- K pattern matching
  K : a : b : r -> reduce (a : r)
  [K, a] -> [K, a]
  [K] -> [K]
  -- S pattern matching
  S : x : y : z : r -> reduce (x : z : Group [y, z] : r)
  [S, a, b] -> [S, a, b]
  [S, a] -> [S, a]
  [S] -> [S]
  -- group reduction
  Group g : rest -> reduce (g ++ rest)
  -- if there's another symbol on the left, leave it and reduce the rest of the
  A : r -> A : reduce r
  B : r -> B : reduce r
  [] -> []
