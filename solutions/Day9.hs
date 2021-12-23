module Day9 where

-- Needed imports
import Data.Char
import Data.Maybe
import qualified Data.Matrix as M

-- Do not change. The main program will access the solutions from this function
solve :: String -> String
solve fcontent = "Solution 1:\t" ++ sol1 ++ "\nSolution 2:\t" ++ sol2 ++ "\n"
    where problemInput = parseFileContent fcontent
          sol1 = (show . s1) problemInput
          sol2 = (show . s2) problemInput

------------------------------------------------------------------------------
-- Change according to the problem
type ProblemInput = M.Matrix Int
-- Parse the input file
parseFileContent :: String -> ProblemInput
parseFileContent = M.fromLists . map (map digitToInt) . lines

sampleInput :: ProblemInput
sampleInput = M.fromLists [[2,1,9,9,9,4,3,2,1,0],
                           [3,9,8,7,8,9,4,9,2,1],
                           [9,8,5,6,7,8,9,8,9,2],
                           [8,7,6,7,8,9,6,7,8,9],
                           [9,8,9,9,9,6,5,6,7,8]]

-- cmpList :: M.Matrix Int -> (Int, Int) -> Int -> Bool
-- cmpList m (i,j) e = all (< e) adjacent
--     where stencil = [(0, 1), (0, -1), (1, 0), (-1, 0)]
--           adjacent = catMaybes [M.safeGet (i+i') (j+j') m | (i', j') <- stencil]

riskLevel :: M.Matrix Int -> (Int, Int) -> Int -> Int
riskLevel m (i,j) e
    | all (> e) adjacent = e+1
    | otherwise          = 0
    where stencil = [(0, 1), (0, -1), (1, 0), (-1, 0)]
          adjacent = catMaybes [M.safeGet (i+i') (j+j') m | (i', j') <- stencil]

-- Solve the first part
s1 :: ProblemInput -> Int
s1 pi = (sum . M.toList) $ M.mapPos (riskLevel pi) pi

-- Solve the second part
s2 :: ProblemInput -> Int
s2 problemInput = -1
