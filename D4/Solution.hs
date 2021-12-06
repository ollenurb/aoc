import Data.List
import Data.Maybe

main :: IO ()
main = undefined

type Board a = [[Maybe a]]

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

-- Solution

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

findWinner :: [Int] -> [Board Int] -> Maybe(Int, Board Int)
findWinner [] _ = Nothing
findWinner (x:xs) boards
    | isJust winningBoard = Just(x, fromJust winningBoard)
    | otherwise = findWinner xs signedBoards
    where
        signedBoards = map (signBoard x) boards
        winningBoard = find isWin signedBoards

sumUnmarked :: Board Int -> Int
sumUnmarked = sum . map (sum . catMaybes)

s1 :: [Int] -> [Board Int] -> Maybe Int
s1 rndseq boards = (\(x, b)->x * sumUnmarked b) <$> findWinner rndseq boards

