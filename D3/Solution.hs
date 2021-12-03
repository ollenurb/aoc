import Data.List

type BinSeq = [Bool]

main :: IO ()
main = interact solve

solve :: String -> String
solve xs = "First solution: " ++ show (s1 binSeq) ++ "\nSecond solution: " ++ show (s2 binSeq)
    where binSeq = (map readBinSeq . lines) xs

readBinSeq :: String -> [Bool]
readBinSeq = map readBit
    where
        readBit '0' = False
        readBit '1' = True

mostCommon :: [Bool] -> Bool
mostCommon bs = (count True bs) >= (count False bs)
    where count a = length . filter (== a)

seqToInt :: [Bool] -> Int
seqToInt bits = sum [2^exp | (bit, exp) <- zip (reverse bits) [0..], bit == True]

s1 :: [BinSeq] -> Int
s1 bs = (seqToInt gammaRate) * (seqToInt epsilonRate)
    where
        gammaRate = (map mostCommon . transpose) bs
        epsilonRate = map not gammaRate

s2 :: [BinSeq] -> Int
s2 xs = (seqToInt $ oxygenRating xs) * (seqToInt $ co2Rating xs)

searchRating :: (Bool -> Bool -> Bool) -> Int -> [BinSeq] -> [BinSeq]
searchRating _   _ [x] = [x]
searchRating cmp i xs  = searchRating cmp (i+1) $ remaining
    where
        remaining = filter ((cmp mc) . (!! i)) xs
        ithMostCommon = mostCommon . (!! i) . transpose
        mc = ithMostCommon xs

oxygenRating :: [BinSeq] -> BinSeq
oxygenRating = head . searchRating (==) 0

co2Rating :: [BinSeq] -> BinSeq
co2Rating = head . searchRating (/=) 0
