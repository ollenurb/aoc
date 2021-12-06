module Day2 where

data Direction = Up
               | Down
               | Forward
    deriving (Show, Eq)

type Command = (Direction, Int)

-- Do not change. The main program will access the solutions from this function
solve :: String -> String
solve fcontent = "Solution 1:\t" ++ sol1 ++ "\nSolution 2:\t" ++ sol2 ++ "\n"
    where problemInput = parseFileContent fcontent
          sol1 = (show . s1) problemInput
          sol2 = (show . s1) problemInput

------------------------------------------------------------------------------
-- Change according to the problem
type ProblemInput = [Command]

-- Parse the input file
parseFileContent :: String -> ProblemInput
parseFileContent = map parseLine . lines

parseDirection :: String -> Direction
parseDirection dirStr
    | dirStr == "up"      = Up
    | dirStr == "down"    = Down
    | dirStr == "forward" = Forward
    | otherwise           = undefined

parseLine :: String -> Command
parseLine line = (parseDirection dirStr, read qnt)
    where [dirStr, qnt] = words line

-- Solve the first part
s1 :: ProblemInput -> Int
s1 = uncurry (*) . foldr executeCommand (0, 0)
    where
        executeCommand :: Command -> (Int, Int) -> (Int, Int)
        executeCommand (cmd, qnt) (hp, vp) =
            case cmd of
              Up      -> (hp, vp - qnt)
              Down    -> (hp, vp + qnt)
              Forward -> (hp + qnt, vp)

-- Solve the second part
s2 :: ProblemInput -> Int
s2 cmds = hp * vp
    where
        (hp, vp, _) = foldl (flip executeCommand') (0, 0, 0) cmds
        executeCommand' (cmd, qnt) (hp, vp, aim) =
            case cmd of
              Up      -> (hp, vp, aim - qnt)
              Down    -> (hp, vp, aim + qnt)
              Forward -> (hp + qnt, vp + (aim * qnt), aim)
