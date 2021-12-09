module Day5 where

-- Needed imports
import Data.Tuple (swap)
import Data.List (group)
import qualified Data.HashMap as HM

-- Do not change. The main program will access the solutions from this function
solve :: String -> String
solve fcontent = "Solution 1:\t" ++ sol1 ++ "\nSolution 2:\t" ++ sol2 ++ "\n"
    where problemInput = parseFileContent fcontent
          sol1 = (show . s1) problemInput
          sol2 = (show . s1) problemInput

------------------------------------------------------------------------------
data Line = Line { getStart :: (Int, Int), getEnd :: (Int, Int) }
    deriving (Show, Eq)

sampleLines :: [Line]
sampleLines = [
    Line (0,9) (5,9),
    Line (8,0) (0,8),
    Line (9,4) (3,4),
    Line (2,2) (2,1),
    Line (7,0) (7,4),
    Line (6,4) (2,0),
    Line (0,9) (2,9),
    Line (3,4) (1,4),
    Line (0,0) (8,8),
    Line (5,5) (8,2)]

-- Change according to the problem
type ProblemInput = [Line]

-- Parse the input file
parseFileContent :: String -> ProblemInput
parseFileContent = map (parseLine . words) . lines
    where parseLine [s, "->", e] = Line (read $ parenthesize s) (read $ parenthesize e)
          parseLine _            = error "Malformed line found"
          parenthesize x = "(" ++ x ++ ")"

-- TODO: To remove
testSolve :: IO Int
testSolve = do
    fileContent <- readFile "inputs/Day5.txt"
    (return . s1 . parseFileContent) fileContent

genPoints :: Line -> [(Int, Int)]
genPoints (Line (x, y) (x', y'))
  | x == x' && y > y' = zip (repeat x) [y'..y]
  | x == x' && y < y' = zip (repeat x) [y..y']
  | y == y' && x > x' = zip [x'..x] (repeat y)
  | y == y' && x < x' = zip [x..x'] (repeat y)
  | otherwise = [(x, y), (x', y')]

isDiagonal :: Line -> Bool
isDiagonal (Line (x, y) (x', y')) = x /= x' && y /= y'

-- Solve the first part
s1 :: ProblemInput -> Int
s1 problemInput = length $ filter ((>= 2) . snd) nOverlaps
    where hvLines = filter (not . isDiagonal) problemInput
          linesPoints = hvLines >>= genPoints
          nOverlaps = (HM.toList . groupAll) linesPoints
          groupAll = foldr (\x->HM.insertWith (+) x 1) HM.empty

-- Solve the second part
s2 :: ProblemInput -> Int
s2 problemInput = -1
