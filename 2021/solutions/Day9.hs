module Day9 where

-- Needed imports
import Data.Char
import Data.Maybe
import Data.List (sortBy)
import Data.Ord
import qualified Data.Set as S
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
type HeightMap = M.Matrix Int
type Coord = (Int, Int)

-- Parse the input file
parseFileContent :: String -> ProblemInput
parseFileContent = M.fromLists . map (map digitToInt) . lines

allowedDirs :: [Coord]
allowedDirs = [(0, 1), (0, -1), (1, 0), (-1, 0)]

-- Given an element and its position, returns if it is a low point or not
isLowPoint :: HeightMap -> Coord -> Int -> Bool
isLowPoint m (i, j) e = all (> e) adjacent
    where adjacent = catMaybes [M.safeGet (i+i') (j+j') m | (i', j') <- allowedDirs]

-- Find low points
findLowPoints :: HeightMap -> [Coord]
findLowPoints m = (catMaybes  . M.toList . M.mapPos (collect m)) m
    where collect m p e = if isLowPoint m p e then Just p else Nothing

-- Solve the first part
s1 :: ProblemInput -> Int
s1 pi = foldr f 0 $ findLowPoints pi
    where f (i, j) a = M.getElem i j pi + 1 + a

basin :: HeightMap -> S.Set Coord -> Coord -> S.Set Coord
basin m s (i, j)
  | e == 9    = s
  | otherwise = (i, j) `S.insert` curBasin
  where adjacentPos = (S.fromList [(i+i', j+j') | (i', j') <- allowedDirs]) S.\\ s
        e = fromMaybe 9 $ M.safeGet i j m
        m' = M.setElem 9 (i, j) m
        curBasin = foldl evalNeighbs S.empty adjacentPos
        evalNeighbs a p = a `S.union` basin m' (a `S.union` s) p

s2 :: ProblemInput -> Int
s2 pi = product $ take 3 (basinsSort lps)
    where lps = findLowPoints pi
          basinsSort = reverseSort . map (S.size . basin pi S.empty)
          reverseSort = sortBy (\x y -> Down x `compare` Down y)
