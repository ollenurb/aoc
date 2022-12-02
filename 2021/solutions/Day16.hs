module Day16 where

import Data.Char
import Control.Monad

-- Needed imports

-- Do not change. The main program will access the solutions from this function
solve :: String -> String
solve fcontent = "Solution 1:\t" ++ sol1 ++ "\nSolution2:\t" ++ sol2 ++ "\n"
    where problemInput = parseInput fcontent
          sol1 = (show . s1) problemInput
          sol2 = (show . s1) problemInput

------------------------------------------------------------------------------
-- | Change according to the problem
newtype BinSeq = BinSeq [Bool]
type ProblemInput = BinSeq

instance Show BinSeq where
    show (BinSeq bs) = map (\x -> if x then '1' else '0') bs

-- | Parse the input file
parseInput :: String -> ProblemInput
parseInput input = binSequence
    where binSequence = BinSeq $ input >>= (intToBin . digitToInt)

printParsed :: String -> String
printParsed = show . BinSeq . (=<<) hexToBin
    where hexToBin = take 4 . (++ repeat False) . intToBin . digitToInt

intToBin :: Int -> [Bool]
intToBin 0 = [False]
intToBin 1 = [True]
intToBin v = toEnum r : intToBin q
    where (q, r) = quotRem v 2



-- | Solve the first part
s1 :: ProblemInput -> Int
s1 problemInput = -1

-- | Solve the second part
s2 :: ProblemInput -> Int
s2 problemInput = -1
