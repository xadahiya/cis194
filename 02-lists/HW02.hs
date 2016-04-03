{-# OPTIONS_GHC -Wall #-}
module HW02 where
-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches a b = sum [1 | (x,y) <-xs, x==y]
  where xs = zip a b

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
findn :: Peg -> Code -> Int
findn a xs = sum [1 | x <-xs , x ==a]

countColors :: Code -> [Int]
countColors xs = [findn a xs | a <- colors]

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches test code = sum [if x<y then x else y | (x,y) <-xs]
    where xs = zip (countColors test) (countColors code)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove = undefined

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent = undefined

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = undefined

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes = undefined

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
