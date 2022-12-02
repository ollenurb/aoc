module Day3 where

-- Needed imports
import Data.List

-- Do not change. The main program will access the solutions from this function
solve :: String -> String
solve fcontent = "Solution 1:\t" ++ sol1 ++ "\nSolution2:\t" ++ sol2 ++ "\n"
    where problemInput = parseFileContent fcontent
          sol1 = (show . s1) problemInput
          sol2 = (show . s1) problemInput

------------------------------------------------------------------------------
-- Change according to the problem
type BinSeq = [Bool]

type ProblemInput = [BinSeq]

-- Parse the input file
parseFileContent :: String -> ProblemInput
parseFileContent = map readBinSeq . lines

readBinSeq :: String -> [Bool]
readBinSeq = map readBit
    where
        readBit '0' = False
        readBit '1' = True
        readBit _   = undefined -- triggers runtime exception

-- Solve the first part
s1 :: ProblemInput -> Int
s1 bs = seqToInt gammaRate * seqToInt epsilonRate
    where
        gammaRate = (map mostCommon . transpose) bs
        epsilonRate = map not gammaRate

mostCommon :: [Bool] -> Bool
mostCommon bs = count True bs >= count False bs
    where count a = length . filter (== a)

seqToInt :: [Bool] -> Int
seqToInt bits = sum [2^exp | (bit, exp) <- zip (reverse bits) [0..], bit]

-- Solve the second part
s2 :: ProblemInput -> Int
s2 xs = seqToInt (oxygenRating xs) * seqToInt (co2Rating xs)

searchRating :: (Bool -> Bool -> Bool) -> Int -> [BinSeq] -> [BinSeq]
searchRating _   _ [x] = [x]
searchRating cmp i xs  = searchRating cmp (i+1) remaining
    where
        remaining = filter (cmp mc . (!! i)) xs
        ithMostCommon = mostCommon . (!! i) . transpose
        mc = ithMostCommon xs

oxygenRating :: [BinSeq] -> BinSeq
oxygenRating = head . searchRating (==) 0

co2Rating :: [BinSeq] -> BinSeq
co2Rating = head . searchRating (/=) 0
