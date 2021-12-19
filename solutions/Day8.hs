module Day8 where

-- Needed imports
import qualified Data.HashSet as HS
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

-- Do not change. The main program will access the solutions from this function
solve :: String -> String
solve fcontent = "Solution 1:\t" ++ sol1 ++ "\nSolution 2:\t" ++ sol2 ++ "\n"
    where problemInput = parseFileContent fcontent
          sol1 = (show . s1) problemInput
          sol2 = (show . s2) problemInput

------------------------------------------------------------------------------
-- Change according to the problem
type ProblemInput = [Entry]

-- Cahnge Set type synonym
type Set a = HS.HashSet a
type Mapping = [(Set Char, Int)]
data Entry = Entry [Set Char] [Set Char]
    deriving (Show, Eq)

-- Parse the input file
parseFileContent :: String -> ProblemInput
parseFileContent = map (entryFromList . map (map HS.fromList . words) . splitOn "|") . lines
    where entryFromList [p, s] = Entry p s

crossCombine :: Entry -> [Bool]
crossCombine (Entry p s) = do
    p' <- filter isAdmittedDigit p
    combined <- zip (repeat p') s
    let res = uncurry (==) combined
    return res
        where isAdmittedDigit x = let sz = HS.size x in sz == 7 || sz == 4 || sz == 3 || sz == 2

-- Solve the first part
s1 :: ProblemInput -> Int
s1 = sum . map (length . filter (==True) . crossCombine)

findMapping :: Entry -> Mapping
findMapping (Entry p _) = zip digits [0..]
    where digits5 = filter ((== 5) . HS.size) p
          digits6 = filter ((== 6) . HS.size) p
          [zero]  = filter ((== 2) . HS.size . flip HS.difference five) digits6
          [one]   = filter ((== 2) . HS.size) p
          [two]   = filter ((== 3) . HS.size . flip HS.difference four) digits5
          [three] = filter ((== 1) . HS.size . flip HS.difference two) digits5
          [four]  = filter ((== 4) . HS.size) p
          [five]  = filter ((== 2) . HS.size . flip HS.difference two) digits5
          [six]   = filter ((== 4) . HS.size . flip HS.difference seven) digits6
          [seven] = filter ((== 3) . HS.size) p
          [eight] = filter ((== 7) . HS.size) p
          [nine]  = filter ((== 2) . HS.size . flip HS.difference four) digits6
          digits  = [zero, one, two, three, four, five, six, seven, eight, nine]

decode :: Entry -> Int
decode e@(Entry p s) = foldl (\a d-> a*10 + d) 0 digits
    where mapping = findMapping e
          digits = fromJust $ mapM (`lookup` mapping) s

-- Solve the second part
s2 :: ProblemInput -> Int
s2 = sum . map decode
