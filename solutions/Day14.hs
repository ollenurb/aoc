module Day14 where

-- Needed imports
import Data.HashMap.Strict (HashMap, fromList, (!), empty, insertWith, elems, foldrWithKey)
import qualified Data.HashMap.Strict as HS
import Data.Hashable (Hashable)

-- Do not change. The main program will access the solutions from this function
solve :: String -> String
solve fcontent = "Solution 1:\t" ++ sol1 ++ "\nSolution2:\t" ++ sol2 ++ "\n"
    where problemInput = parseFileContent fcontent
          sol1 = (show . s1) problemInput
          sol2 = (show . s1) problemInput

------------------------------------------------------------------------------
-- Change according to the problem
type ProblemInput = (Rules, Histogram String)
type Rules = HashMap String Char
type Histogram a = HashMap a Int

-- Parse the input file
parseFileContent :: String -> ProblemInput
parseFileContent fc = (rulesMap, parseTemplate template)
    where (template:"":rules) = lines fc
          rulesMap = fromList $ map parseRule rules

parseRule :: String -> (String, Char)
parseRule s = (k, head v)
    where [k, "->", v] = words s

parseTemplate :: String -> Histogram String
parseTemplate str = foldr insertHistogram empty $ zipWith (\a b -> [a,b]) str (tail str)

insertHistogram :: (Hashable k) => k -> Histogram k -> Histogram k
insertHistogram = flip (insertWith (+)) 1

-- mapping: mapping function
-- mf: merge function
concatMapWith :: (Hashable k) => (v -> v -> v) -> (k -> [k]) -> HashMap k v -> HashMap k v
concatMapWith mf mapping = foldrWithKey folder empty
    where folder k v m = foldr (flip (insertWith mf) v) m (mapping k)

-- In order to make it  more efficient we can represent a single string with an
-- histogram of paired characters (or rules). When we apply a rule we update
-- the histogram accordingly.
-- Suppose we have NNCB -> NN = 1, NC = 1, CB = 1.
-- When we apply the rule we obtain NC = 1, CN =1, NB = 1, BC = 1, CH = 1, HB = 1.
-- By doing this the complexity is linear with respect to the rules list (worst case)
step :: Rules -> Histogram String -> Histogram String
step rules = concatMapWith (+) (outcomes rules)

-- Given a set of rules and a string, compute the resulting pairs
outcomes :: Rules -> String -> [String]
outcomes rules k@[f, s] = [[f, c], [c, s]]
    where c = rules ! k

computeStatistics :: Histogram String -> Histogram String
computeStatistics = HS.map ((`div` 2) . (+1)) . concatMapWith (+) mapping
    where mapping [f, s] = [[f], [s]]

mclc :: Histogram String -> (Int, Int)
mclc hs = (maximum stats, minimum stats)
    where stats = (elems . computeStatistics) hs

-- Solve the first part
s1 :: ProblemInput -> Int
s1 (rules, template) = uncurry (-) $ mclc $ iterate (step rules) template !! 10

-- Solve the second part
s2 :: ProblemInput -> Int
s2 (rules, template) = uncurry (-) $ mclc $ iterate (step rules) template !! 40
