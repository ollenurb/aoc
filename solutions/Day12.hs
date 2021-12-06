module Day12 where

-- Needed imports

-- Do not change. The main program will access the solutions from this function
solve :: String -> String
solve fcontent = "Solution 1:\t" ++ sol1 ++ "\nSolution2:\t" ++ sol2 ++ "\n"
    where problemInput = parseFileContent fcontent
          sol1 = (show . s1) problemInput
          sol2 = (show . s1) problemInput

------------------------------------------------------------------------------
-- Change according to the problem
type ProblemInput = String

-- Parse the input file
parseFileContent :: String -> ProblemInput
parseFileContent = undefined

-- Solve the first part
s1 :: ProblemInput -> Int
s1 problemInput = -1

-- Solve the second part
s2 :: ProblemInput -> Int
s2 problemInput = -1
