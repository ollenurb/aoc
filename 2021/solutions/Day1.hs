module Day1 where

-- Needed imports

-- Do not change. The main program will access the solutions from this function
solve :: String -> String
solve fcontent = "Solution 1:\t" ++ sol1 ++ "\nSolution2:\t" ++ sol2 ++ "\n"
    where problemInput = parseFileContent fcontent
          sol1 = (show . s1) problemInput
          sol2 = (show . s1) problemInput

------------------------------------------------------------------------------
type ProblemInput = [Int]

-- Parse the input file
parseFileContent :: String -> ProblemInput
parseFileContent = map read . lines

-- Solve the first part
s1 :: ProblemInput -> Int
s1 xs@(_:xxs) = length $ filter (==True) $ zipWith (>) xxs xs
s1 [] = -1

-- Solve the second part
s2 :: ProblemInput -> Int
s2 = s1 . triSums

triSums :: [Int] -> [Int]
triSums [] = []
triSums xs@(_:xxs) = zipWith3 sum3 xs xxs xxxs
    where
        xxxs = tail xxs
        sum3 x y z = x + y + z
