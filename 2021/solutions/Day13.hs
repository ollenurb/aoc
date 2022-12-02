module Day13 where

-- Needed imports
import Data.List (maximum)
import qualified Data.HashSet as HS
import Data.List.Split (splitOn)

-- Do not change. The main program will access the solutions from this function
solve :: String -> String
solve fcontent = "Solution 1:\t" ++ sol1 ++ "\nSolution 2:\n" ++ sol2 ++ "\n"
    where problemInput = parseFileContent fcontent
          sol1 = (show . s1) problemInput
          sol2 = (render . s2) problemInput

------------------------------------------------------------------------------
-- Change according to the problem
type ProblemInput = (Paper, [Command])
type Paper   = HS.HashSet (Int, Int)
data Axis    = X | Y deriving (Show, Eq)
data Command = Command Axis Int
    deriving (Show, Eq)

foldPaper :: Command -> Paper -> Paper
foldPaper command = HS.map (translate command)
    where
        translate (Command Y split) p@(x, y) = if y > split then (x, (2 * split) - y) else p
        translate (Command X split) p@(x, y) = if x > split then ((2 * split) - x, y) else p

-- Parse the input file
parseFileContent :: String -> ProblemInput
parseFileContent content = (HS.fromList $ map parsePoint paper, map parseCommand commands)
    where [paper, commands] = splitOn [""] . lines $ content

parsePoint :: String -> (Int, Int)
parsePoint x = read $ "(" ++ map transform x ++ ")"
    where transform x = if x == '-' then ',' else x

parseCommand :: String -> Command
parseCommand = parseWord . last . words
    where parseWord (d:'=':n) = Command (mkAxis d) (read n)
          mkAxis 'x' = X
          mkAxis 'y' = Y

-- Solve the first part
s1 :: ProblemInput -> Int
s1 (paper, commands) = length $ foldPaper (head commands) paper

-- Solve the second part
-- Here you need to render the resulting fold
s2 :: ProblemInput -> Paper
s2 (paper, commands) = foldl (flip foldPaper) paper commands

render :: Paper -> String
render p = unlines [[if (x, y) `HS.member` p then '#' else ' ' | x <- [0..xMax]] | y <- [0..yMax]]
    where
        (xMax, yMax) = (maximum . HS.map fst $ p, maximum . HS.map snd $ p)
