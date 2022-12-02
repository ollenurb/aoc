module Day12 where

-- Needed imports
import Data.HashMap.Lazy (HashMap, (!), findWithDefault, fromList, insertWith, empty)
import Data.Hashable
import Data.Char (isLower)
import Data.List ((\\))
import Data.List.Split (splitOn)

import Control.Monad.Reader (Reader)

import Control.Monad.State (State, modify, get, when, gets, evalState)

-- Do not change. The main program will access the solutions from this function
solve :: String -> String
solve fcontent = "Solution 1:\t" ++ sol1 ++ "\nSolution 2:\t" ++ sol2 ++ "\n"
    where problemInput = parseFileContent fcontent
          sol1 = (show . s1) problemInput
          sol2 = (show . s1) problemInput

------------------------------------------------------------------------------
-- Change according to the problem
data Node = Small String
          | Big String
          | Start
          | End
    deriving (Eq, Show)

type Graph k = HashMap k [k] -- Represent a graph as an adjacency list
type ProblemInput = Graph Node
type Histogram a = HashMap a Int

-- In order to make nodes into a Graph we need to implement an Hashable
-- typeclass instance
instance Hashable Node where
    hashWithSalt i n = hashWithSalt i (show n)

isSmall :: Node -> Bool
isSmall (Small _) = True
isSmall _         = False

countPaths :: Int -> Graph Node -> Histogram Node -> Node -> Int
countPaths _        _     _    End  = 1
countPaths maxSmall graph hist node
    | counts < maxSmall = sum $ map (countPaths maxSmall' graph (visited hist)) reachable
    | otherwise         = 0
    where counts    = findWithDefault 0 node hist
          reachable = findWithDefault [] node graph
          visited   = if isSmall node then insertWith (+) node 1 else id
          maxSmall' = if counts == (maxSmall-1) then 1 else maxSmall


-- Parse the input file
parseFileContent :: String -> ProblemInput
parseFileContent fc = foldr (uncurry $ insertWith (++)) empty processed
    where processed = lines fc >>= preProcess

parseNode :: String -> Node
parseNode s
  | s == "start"  = Start
  | s == "end"    = End
  | all isLower s = Small s
  | otherwise     = Big s

-- Pre-process the input file
preProcess :: String -> [(Node, [Node])]
preProcess s = filter malformed [(f, [t]), (t, [f])]
    where [f, t] = (map parseNode .  splitOn "-") s
          malformed (f, [t]) = f /= End && t /= Start

-- Solve the first part
s1 :: ProblemInput -> Int
s1 problemInput = countPaths 1 problemInput empty Start

-- Solve the second part
s2 :: ProblemInput -> Int
s2 problemInput = countPaths 2 problemInput empty Start
