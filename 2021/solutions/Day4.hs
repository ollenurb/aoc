module Day4 where

-- Needed imports
import Data.List
import Data.Maybe
import Data.List.Split (splitOn, splitWhen)

-- Do not change. The main program will access the solutions from this function
solve :: String -> String
solve fcontent = "Solution 1:\t" ++ sol1 ++ "\nSolution 2:\t" ++ sol2 ++ "\n"
    where problemInput = parseFileContent fcontent
          sol1 = (show . s1) problemInput
          sol2 = (show . s2) problemInput

------------------------------------------------------------------------------
-- Change according to the problem
type Board a = [[Maybe a]]

type ProblemInput = ([Int], [Board Int])

-- Parse the input file
parseFileContent :: String -> ProblemInput
parseFileContent inputFile = (inSeq, boards)
    where inputLines = lines inputFile
          inSeq = map strtoi . splitOn "," . head $ inputLines
          boards = (map . map) (map (Just . strtoi)  . words) strBoards
          strBoards = splitWhen (=="") . tail $ inputLines
          strtoi x = read x :: Int

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

-- Find the every winning board in a board list given a sequence of number
findWinners :: [Int] -> [Board Int] -> [(Int, Board Int)]
findWinners _ [] = []
findWinners [] _ = []
findWinners (x:xs) boards = zip (repeat x) winningBoards ++ findWinners xs losingBoards
    where signedBoards = map (signBoard x) boards
          losingBoards = filter (not . isWin) signedBoards
          winningBoards = filter isWin signedBoards

boardScore :: Int -> Board Int -> Int
boardScore x board = x * sumUnmarked board
    where sumUnmarked = sum . map (sum . catMaybes)

--- Solve the first part
s1 :: ProblemInput -> Int
s1 (xs, boards) = boardScore n b
    where (n, b) = head $ findWinners xs boards

-- Solve the second part
s2 :: ProblemInput -> Int
s2 (xs, boards) = boardScore n b
    where (n, b) = last $ findWinners xs boards

-- TODO: Here are some improvements that can be done
--    * findWinners can actually return a list of scores by computing scores
--      beforehand
--    * Input parsing can be done with a Parser combinator library
