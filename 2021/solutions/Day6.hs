module Day6 where

-- Needed imports
import Data.List.Split (splitWhen)
import Data.List

-- Do not change. The main program will access the solutions from this function
solve :: String -> String
solve fcontent = "Solution 1:\t" ++ sol1 ++ "\nSolution 2:\t" ++ sol2 ++ "\n"
    where problemInput = parseFileContent fcontent
          sol1 = (show . s1) problemInput
          sol2 = (show . s2) problemInput

------------------------------------------------------------------------------
-- Change according to the problem
type ProblemInput = [Int]
type Histogram a = [(Int, a)]

hist :: (Ord a) => [a] -> Histogram a
hist = map (\x->(length x, head x)) . group . sort

-- TODO: Can be decluttered with an hashmap
newGen :: Histogram Int -> Histogram Int
newGen hs = (map reduceDuplicates . genericGroup) $ hs >>= createNew . fmap pred
    where createNew (n, v)
              | v == -1   = [(n, 6), (n, 8)]
              | otherwise = [(n, v)]
          compareTuple a a' = snd a == snd a'
          genericGroup = groupBy compareTuple . sortOn snd
          reduceDuplicates = foldl (+++) (0,0)

-- Combine together 2 tuples discarding the last second value
(+++) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(a, _) +++ (c, d) = (a + c, d)

-- Parse the input file
parseFileContent :: String -> ProblemInput
parseFileContent = map read . splitWhen (==',')

-- Solve the first part
s1 :: ProblemInput -> Int
s1 = sum . fst . unzip . (!! 80) . iterate newGen . hist

-- Solve the second part
s2 :: ProblemInput -> Int
s2 = sum . fst . unzip . (!! 256) . iterate newGen . hist
