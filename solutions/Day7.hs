module Day7 where

-- Needed imports
import Data.List.Split (splitWhen)

-- Do not change. The main program will access the solutions from this function
solve :: String -> String
solve fcontent = "Solution 1:\t" ++ sol1 ++ "\nSolution 2:\t" ++ sol2 ++ "\n"
    where problemInput = parseFileContent fcontent
          sol1 = (show . s1) problemInput
          sol2 = (show . s2) problemInput

------------------------------------------------------------------------------
-- Change according to the problem
type ProblemInput = [Int]

-- Parse the input file
parseFileContent :: String -> ProblemInput
parseFileContent = map read . splitWhen (==',')

computeCost :: Int -> ProblemInput -> Int
computeCost v = sum . map (abs . (v -))

-- Solve the first part
s1 :: ProblemInput -> Int
s1 pi = minimum costs
    where costs = map (`computeCost` pi) [minimum pi..maximum pi]

computeCost' :: Int -> ProblemInput -> Int
computeCost' v = sum . map (newStep . dist v)
    where dist x y = abs (x - y)
          newStep d = d*(d-1) `div` 2 -- \Sum_{i=1}^n = \frac{n(n-1)}{2}

-- Solve the second part
s2 :: ProblemInput -> Int
s2 pi = minimum costs
    where costs = map (`computeCost'` pi) [minimum pi..maximum pi]
