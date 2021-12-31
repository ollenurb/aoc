module Day13 where

-- Needed imports
import Data.List (nub)
import Data.List.Split (splitOn)

-- Do not change. The main program will access the solutions from this function
solve :: String -> String
solve fcontent = "Solution 1:\t" ++ sol1 ++ "\nSolution2:\t" ++ sol2 ++ "\n"
    where problemInput = parseFileContent fcontent
          sol1 = (show . s1) problemInput
          sol2 = (show . s1) problemInput

------------------------------------------------------------------------------
-- Change according to the problem
type ProblemInput = (Paper, [Command])
type Paper   = [(Int, Int)]
data Axis    = X | Y deriving (Show, Eq)
data Command = Command Axis Int
    deriving (Show, Eq)

testInput :: Paper
testInput = [(6,10),
             (0,14),
             (9,10),
             (0,3),
             (10,4),
             (4,11),
             (6,0),
             (6,12),
             (4,1),
             (0,13),
             (10,12),
             (3,4),
             (3,0),
             (8,4),
             (1,10),
             (2,14),
             (8,10),
             (9,0)]

foldPaper :: Command -> Paper -> Paper
foldPaper command = nub . map (translate command)

translate :: Command -> (Int, Int) -> (Int, Int)
translate (Command Y split) p@(x, y) = if y > split then (x, (2 * split) - y) else p
translate (Command X split) p@(x, y) = if x > split then ((2 * split) - x, y) else p

-- Parse the input file
parseFileContent :: String -> ProblemInput
parseFileContent content = (map parsePoint paper, map parseCommand commands)
    where [paper, commands] = (splitOn [""] . lines) content

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
s2 :: ProblemInput -> Int
s2 (paper, commands) = length $ foldl (flip foldPaper) paper commands

s2' (paper, commands) = foldl (flip foldPaper) paper commands

render :: Paper -> IO ()
render = undefined
