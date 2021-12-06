module Day4 where

-- Needed imports
import Data.List
import Data.Maybe

-- Do not change. The main program will access the solutions from this function
solve :: String -> String
solve fcontent = "Solution 1:\t" ++ sol1 ++ "\nSolution2:\t" ++ sol2 ++ "\n"
    where problemInput = parseFileContent fcontent
          sol1 = (show . s1) problemInput
          sol2 = (show . s1) problemInput

------------------------------------------------------------------------------
-- Change according to the problem
type Board a = [[Maybe a]]

type ProblemInput = ([Int], [Board Int])

-- Testing Stuff - TODO: Remove
inSeq :: [Int]
inSeq = [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]

inBoards :: [Board Int]
inBoards = (map . map . map) Just [
               [
                    [22,13,17,11, 0],
                    [8 ,2 ,23, 4,24],
                    [21, 9,14,16, 7],
                    [6 ,10, 3,18, 5],
                    [ 1,12,20,15,19]
               ],
               [
                    [3,15, 0, 2, 22],
                    [9,18,13,17, 5 ],
                    [19, 8, 7,25,23],
                    [20,11,10,24, 4],
                    [14,21,16,12, 6]
               ],
               [
                    [14,21,17,24, 4],
                    [10,16,15, 9,19],
                    [18, 8,23,26,20],
                    [22,11,13, 6, 5],
                    [2 ,0,12, 3, 7]
               ]
           ]


sampleFileInput :: String
sampleFileInput = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\
\\n\
\22 13 17 11  0\n\
\ 8  2 23  4 24\n\
\21  9 14 16  7\n\
\ 6 10  3 18  5\n\
\ 1 12 20 15 19\n\
\\n\
\ 3 15  0  2 22\n\
\ 9 18 13 17  5\n\
\19  8  7 25 23\n\
\20 11 10 24  4\n\
\14 21 16 12  6\n\
\\n\
\14 21 17 24  4\n\
\10 16 15  9 19\n\
\18  8 23 26 20\n\
\22 11 13  6  5\n\
\ 2  0 12  3  7\n"

-- Parse the input file
parseFileContent :: String -> ProblemInput
parseFileContent = undefined

-- Solve the first part
s1 :: ProblemInput -> Int
s1 (rndseq, boards) = (\(x, b)->x * sumUnmarked b) $ findWinner rndseq boards
    where sumUnmarked = sum . map (sum . catMaybes)

-- Determines if a board is a win board or not
isWin :: Board Int -> Bool
isWin rows = check rows || check cols
    where cols = transpose rows
          check = any (all isNothing)

-- Sign a board with a given number
signBoard :: Int -> Board Int -> Board Int
signBoard x = (map . map) (sign (Just x))
    where sign v v'
            | v == v'   = Nothing
            | otherwise = v'

-- Find the first winning board in a board list given a sequence of number
findWinner :: [Int] -> [Board Int] -> (Int, Board Int)
findWinner [] _ = undefined
findWinner (x:xs) boards
    | isJust winningBoard = (x, fromJust winningBoard)
    | otherwise = findWinner xs signedBoards
    where signedBoards = map (signBoard x) boards
          winningBoard = find isWin signedBoards

-- Solve the second part
s2 :: ProblemInput -> Int
s2 problemInput = -1
