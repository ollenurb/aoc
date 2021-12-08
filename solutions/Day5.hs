module Day5 where

-- Needed imports
import Data.Tuple (swap)

-- Do not change. The main program will access the solutions from this function
solve :: String -> String
solve fcontent = "Solution 1:\t" ++ sol1 ++ "\nSolution 2:\t" ++ sol2 ++ "\n"
    where problemInput = parseFileContent fcontent
          sol1 = (show . s1) problemInput
          sol2 = (show . s1) problemInput

------------------------------------------------------------------------------
data Line = Line { getStart :: (Int, Int), getEnd :: (Int, Int) }
    deriving (Show, Eq)

-- Change according to the problem
type ProblemInput = [Line]

-- Parse the input file
parseFileContent :: String -> ProblemInput
parseFileContent = undefined

-- TODO: Can be refactorized
genVHPoints :: Line -> [(Int, Int)]
genVHPoints (Line (x, y) (x', y'))
  | x == x' && y > y' = zip (repeat x) [y'..y]
  | x == x' && y < y' = zip (repeat x) [y..y']
  | y == y' && x > x' = zip [x'..x] (repeat y)
  | y == y' && x < x' = zip [x..x'] (repeat y)
  | otherwise = [(x, y), (x', y')]

-- Solve the first part
s1 :: ProblemInput -> Int
s1 problemInput = -1

-- Solve the second part
s2 :: ProblemInput -> Int
s2 problemInput = -1
