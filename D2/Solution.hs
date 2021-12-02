import Data.Maybe

data Direction = Up
               | Down
               | Forward
    deriving (Show, Eq)

type Command = (Direction, Int)

main :: IO ()
main = interact solve

solve :: String -> String
solve ss = "First solution is " ++ show (s1 cmds) ++ ", second is " ++ show (s2 cmds)
    where
        cmds = map parseLine . lines $ ss  -- Converted String in a list of Ints

---------------------------------------------------------------
------------------ Annoying parsing stuff ---------------------
---------------------------------------------------------------
parseDirection :: String -> Direction
parseDirection dirStr
    | dirStr == "up"      = Up
    | dirStr == "down"    = Down
    | dirStr == "forward" = Forward

parseLine :: String -> Command
parseLine line = (parseDirection dirStr, read qnt)
    where [dirStr, qnt] = words line

---------------------------------------------------------------
----------- Actual Algorithms to solve the problems -----------
---------------------------------------------------------------
s1 :: [Command] -> Int
s1 = uncurry (*) . foldr executeCommand (0, 0)
    where
        executeCommand :: Command -> (Int, Int) -> (Int, Int)
        executeCommand (cmd, qnt) (hp, vp) =
            case cmd of
              Up      -> (hp, vp - qnt)
              Down    -> (hp, vp + qnt)
              Forward -> (hp + qnt, vp)

s2 :: [Command] -> Int
s2 cmds = hp * vp
    where
        (hp, vp, _) = foldl (flip executeCommand') (0, 0, 0) cmds
        executeCommand' (cmd, qnt) (hp, vp, aim) =
            case cmd of
              Up      -> (hp, vp, aim - qnt)
              Down    -> (hp, vp, aim + qnt)
              Forward -> (hp + qnt, vp + (aim * qnt), aim)
