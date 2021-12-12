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
          sol2 = (show . s2) problemInput

------------------------------------------------------------------------------
data Line = Line { getStart :: (Int, Int), getEnd :: (Int, Int) }
    deriving (Show, Eq)

-- Change according to the problem
type ProblemInput = [Line]

-- Parse the input file
parseFileContent :: String -> ProblemInput
parseFileContent = map (parseLine . words) . lines
    where parseLine [s, "->", e] = Line (read $ parenthesize s) (read $ parenthesize e)
          parseLine _            = error "Malformed line found"
          parenthesize x = "(" ++ x ++ ")"

(...) :: (Ord a, Enum a) => a ->  a -> [a]
a ... b
  | a < b   = [a..b]
  | otherwise = reverse [b..a]

genPoints :: Line -> [(Int, Int)]
genPoints (Line (x, y) (x', y'))
  | y == y'            = zip (x...x') (repeat y)
  | x == x'            = zip (repeat x) (y...y')
  | otherwise          = zip (x...x') (y...y')

isDiagonal :: Line -> Bool
isDiagonal (Line (x, y) (x', y')) = x /= x' && y /= y'

is45Deg :: Line -> Bool
is45Deg (Line (x, y) (x', y')) = abs (x - x') == abs (y - y')

-- Given a list of points, returns an hashmap that indicates how many times
-- a point occur on the original list
groupAll :: [(Int, Int)] -> HM.Map (Int, Int) Int
groupAll = foldr (\x->HM.insertWith (+) x 1) HM.empty

-- Solve the first part
s1 :: ProblemInput -> Int
s1 problemInput = length $ filter ((>= 2) . snd) nOverlaps
    where hvLines = filter (not . isDiagonal) problemInput
          linesPoints = hvLines >>= genPoints
          nOverlaps = (HM.toList . groupAll) linesPoints

-- Solve the second part
s2 :: ProblemInput -> Int
s2 problemInput = length $ filter ((>= 2) . snd) nOverlaps
    where hvLines = filter (not . isDiagonal) problemInput
          lines45 = filter is45Deg problemInput
          linesPoints = (hvLines ++ lines45) >>= genPoints
          nOverlaps = (HM.toList . groupAll) linesPoints
