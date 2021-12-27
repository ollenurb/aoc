module Day11 where

-- Needed imports
import qualified Data.Matrix as M
import Control.Monad.State (State, state, get, gets, put, modify, execState, when, evalState)
import Control.Monad (replicateM)
import Data.Maybe (fromMaybe)
import Data.List (elemIndex)
import Data.Char (digitToInt)


-- Do not change. The main program will access the solutions from this function
solve :: String -> String
solve fcontent = "Solution 1:\t" ++ sol1 ++ "\nSolution 2:\t" ++ sol2 ++ "\n"
    where problemInput = parseFileContent fcontent
          sol1 = (show . s1) problemInput
          sol2 = (show . s2) problemInput

------------------------------------------------------------------------------
-- Change according to the problem
type ProblemInput = M.Matrix Octopus
type Grid = M.Matrix Octopus
type Coord = (Int, Int)
data Octopus = Energy Int | Flash
    deriving (Show, Eq)

-- Parse the input file
parseFileContent :: String -> ProblemInput
parseFileContent = M.fromLists . map (map (Energy . digitToInt)) . lines

stepOctopus :: Octopus -> Octopus
stepOctopus Flash      = Flash
stepOctopus (Energy x) = Energy (x+1)

isFlashing :: Octopus -> Bool
isFlashing (Energy x) = x >= 10
isFlashing _          = False

resetOctopus :: Octopus -> Octopus
resetOctopus Flash = Energy 0
resetOctopus x = x

takeCoords :: (a -> Bool) -> M.Matrix a -> [Coord]
takeCoords p g = [(i, j) | i <- [1..m], j <- [1..n], p $ M.getElem i j g]
    where (m, n) = (M.nrows g, M.ncols g)

neighbors :: Coord -> [Coord]
neighbors (i, j) = [(r, c) | r <- [i-1..i+1], c <- [j-1..j+1], r /= i || j /= c]

mapElem :: (a -> a) -> Coord -> M.Matrix a -> M.Matrix a
mapElem f p g
    | isValid p = M.setElem (f $ (M.!) g p) p g
    | otherwise = g
    where (m, n) = (M.nrows g, M.ncols g)
          isValid (i, j) = i <= m && i >= 1 && j <= n && j >= 1

mapElems :: (a -> a) -> [Coord] -> M.Matrix a -> M.Matrix a
mapElems f cs g = foldl (flip (mapElem f)) g cs

flash :: State Grid Int
flash  = do
    flashing <- gets $ takeCoords isFlashing     -- Take currently flashing Octopuses
    if null flashing then return 0 else do       -- No Flashing octopuses, then stop
        modify $ mapElems (const Flash) flashing -- Set flashing octopuses to Flash(ed)
        let toUpdate = flashing >>= neighbors    -- Take the neighbors of each flashing point
        modify $ mapElems stepOctopus toUpdate   -- Step up
        flashing' <- flash                       -- Re-Flash if necessary
        return (flashing' + length flashing)     -- Return the total number of flashes

step :: State Grid Int
step = do
    modify $ fmap stepOctopus
    s <- flash
    modify $ fmap resetOctopus
    return s

-- Solve the first part
s1 :: ProblemInput -> Int
s1 = countN 100
    where countN n = sum . evalState (replicateM n step)

-- All flashing octopuses should have the number of flashed octopuses equals to
-- the total number of octopuses
-- Solve the second part
s2 :: ProblemInput -> Int
s2 g = fromMaybe (-1) result
    where totOctopuses = M.nrows g * M.ncols g
          counts = evalState (repeatM step) g
          repeatM = sequence . repeat
          result = (+1) <$> elemIndex totOctopuses counts
