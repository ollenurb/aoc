module Day15 where

-- Needed imports
import Data.Matrix (Matrix, nrows, ncols, matrix, (!), setElem)
import qualified Data.Matrix as Matrix
import Control.Monad.State (State, get, put, when, evalState)

import Data.Char (digitToInt)
import Data.PSQueue (PSQ, Binding ((:->)), key)
import qualified Data.PSQueue as Queue

import Debug.Trace (trace)

-- Do not change. The main program will access the solutions from this function
solve :: String -> String
solve fcontent = "Solution 1:\t" ++ sol1 ++ "\nSolution2:\t" ++ sol2 ++ "\n"
    where problemInput = parseFileContent fcontent
          sol1 = (show . s1) problemInput
          sol2 = (show . s1) problemInput

------------------------------------------------------------------------------
-- Change according to the problem
type ProblemInput = Graph
type Graph = Matrix Double
type Vertex = (Int, Int)

type DijkstraState = (Graph, PSQ Vertex Double, Matrix Double, Matrix (Maybe Vertex))
-- Dijkstra function state
--    - Graph
--    - Priority queue
--    - Dist
--    - Prev
type SolverState a = State DijkstraState a

sampleInput :: Graph
sampleInput = Matrix.fromLists
               [[1,1,6,3,7,5,1,7,4,2],
                [1,3,8,1,3,7,3,6,7,2],
                [2,1,3,6,5,1,1,3,2,8],
                [3,6,9,4,9,3,1,5,6,9],
                [7,4,6,3,4,1,7,1,1,1],
                [1,3,1,9,1,2,8,1,3,7],
                [1,3,5,9,9,1,2,4,2,1],
                [3,1,2,5,4,2,1,6,3,9],
                [1,2,9,3,1,3,8,5,2,1],
                [2,3,1,1,9,4,4,5,8,1]]

neighbors :: Graph -> Vertex -> [Vertex]
neighbors g (i, j) = filter checkBounds [(i-1, j), (i+1, j), (i, j+1)]
    where (r, c) = (nrows g, ncols g)
          checkBounds (i', j') = i' <= r && i' >= 1 && j' <= c && j' >= 1

bestPath :: Graph -> Vertex -> Vertex -> [Vertex]
bestPath graph source target = takePath target runDijkstra
    where (r, c) = (nrows graph, ncols graph)
          dist = setElem 0 source $ matrix r c (const infinity)
          prev = matrix r c (const Nothing)
          heap = Queue.fromList [(i, j) :-> infinity | i <- [1..r], j <- [1..c]]
          runDijkstra = evalState (dijkstra target) (graph, heap, dist, prev)

dijkstra :: Vertex -> SolverState (Matrix (Maybe Vertex))
dijkstra target = do
    (graph, heap, dist, prev) <- get
    case Queue.minView heap of
      Nothing -> return prev
      Just (u, heap') -> if key u == target then return prev else do
        let neighs = neighbors graph $ key u
        put (graph, heap', dist, prev)
        mapM_ (loop $ key u) neighs
        dijkstra target

loop :: Vertex -> Vertex -> SolverState ()
loop u v = do
    (graph, heap, dist, prev) <- get
    let alt = (dist ! u) + (graph ! v)
    when (alt < dist ! v) $ do
        let dist' = setElem alt v dist
        let prev' = setElem (Just u) v prev
        let heap' = Queue.adjust (\p -> p - alt) v heap
        put (graph, heap', dist', prev')

takePath :: Vertex -> Matrix (Maybe Vertex) -> [Vertex]
takePath v m = case m ! v of
                 Nothing -> []
                 Just v' -> v':takePath v' m

infinity :: Double
infinity = read "Infinity"

-- Parse the input file
parseFileContent :: String -> ProblemInput
parseFileContent = Matrix.fromLists . map (map toDouble) . lines
    where toDouble x = fromIntegral (digitToInt x) :: Double

-- Solve the first part
s1 :: ProblemInput -> Int
s1 pi = floor $ sum $ map (pi !) $ bestPath pi (1,1) (r, c)
    where (r, c) = (nrows pi, ncols pi)

-- Solve the second part
s2 :: ProblemInput -> Int
s2 problemInput = -1
