module Day15 where

-- Needed imports
import           Data.Matrix            (Matrix, nrows, ncols, matrix, (!), setElem)
import qualified Data.Matrix as Matrix
import           Control.Monad.State    (State, get, put, when, evalState)
import           Data.Char              (digitToInt)
import           Data.PSQueue           (PSQ, Binding ((:->)))
import qualified Data.PSQueue as Queue


-- Do not change. The main program will access the solutions from this function
solve :: String -> String
solve fcontent = "Solution 1:\t" ++ sol1 ++ "\nSolution 2:\t" ++ sol2 ++ "\n"
    where problemInput = parseFileContent fcontent
          sol1 = (show . s1) problemInput
          sol2 = (show . s2) problemInput

------------------------------------------------------------------------------
-- Change according to the problem
type ProblemInput = Graph
type Graph = Matrix Int
type Vertex = (Int, Int)
type Infinitable = Infinite Int

type DijkstraState = (Graph, PSQ Vertex Infinitable, Matrix Infinitable)
type SolverState a = State DijkstraState a

data Infinite a = Infinity
                | Number a
    deriving (Eq, Show)

instance Functor Infinite where
    fmap f (Number a) = Number $ f a
    fmap f Infinity   = Infinity

instance (Num a) => Num (Infinite a) where
    (+) (Number a) b = fmap (+a) b
    (+) a b          = (+) b a

instance (Ord a) => Ord (Infinite a) where
    (<=) Infinity _            = False
    (<=) _ Infinity            = True
    (<=) (Number a) (Number b) = a <= b

unwrap :: Infinite a -> a
unwrap (Number a) = a
unwrap Infinity = error "Cannot unwrap infinity values"

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
neighbors g (i, j) = filter checkBounds [(i-1, j), (i+1, j), (i, j+1), (i, j-1)]
    where (r, c) = (nrows g, ncols g)
          checkBounds (i', j') = i' <= r && i' >= 1 && j' <= c && j' >= 1

bestCost :: Graph -> Vertex -> Vertex -> Int
bestCost graph source target = unwrap $ runDijkstra ! target
    where
        (r, c) = (nrows graph, ncols graph)
        dist = setElem (Number 0) source $ matrix r c (const Infinity)
        heap = Queue.fromList [(i, j) :-> (dist ! (i, j)) | i <- [1..r], j <- [1..c]]
        runDijkstra = evalState (dijkstra target) (graph, heap, dist)

dijkstra :: Vertex -> SolverState (Matrix Infinitable)
dijkstra target = do
    (graph, heap, dist) <- get
    case Queue.minView heap of
      Nothing -> return dist
      Just (u :-> _, heap') -> if u == target then return dist else do
        let neighs = neighbors graph u
        put (graph, heap', dist)
        mapM_ (loop u) neighs
        dijkstra target

loop :: Vertex -> Vertex -> SolverState ()
loop u v = do
    (graph, heap, dist) <- get
    let alt = (dist ! u) + Number (graph ! v)
    when (alt < dist ! v) $ do
        let dist' = setElem alt v dist
        let heap' = Queue.adjust (decreasePrio alt) v heap
        put (graph, heap', dist')

decreasePrio :: Infinitable -> Infinitable -> Infinitable
decreasePrio v Infinity = v
decreasePrio a b        = b - a

-- Parse the input file
parseFileContent :: String -> ProblemInput
parseFileContent = Matrix.fromLists . map (map digitToInt) . lines

-- Solve the first part
s1 :: ProblemInput -> Int
s1 pi = bestCost pi (1,1) (r, c)
    where (r, c) = (nrows pi, ncols pi)

inflate :: Matrix Int -> Matrix Int
inflate m = Matrix.flatten mapped
    where stencil = Matrix.fromLists [[fromIntegral (i + j) | j <- [0..4]] | i <- [0..4]]
          mapped = (\x-> f x <$> m) <$> stencil
          f x y = uncurry (+) $ quotRem (x + y) 10

-- Solve the second part
s2 :: ProblemInput -> Int
s2 pi = bestCost (inflate pi) (1,1) (r, c)
    where (r, c) = (nrows pi, ncols pi)
