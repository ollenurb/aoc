main :: IO ()
main = interact solve

solve :: String -> String
solve ss = "First solution is " ++ show (s1 intsList) ++ ", second is " ++ show (s2 intsList)
    where
        strtoi s = read s :: Int            -- Convert string to Int
        intsList = map strtoi . lines $ ss  -- Converted String in a list of Ints

s1 :: [Int] -> Int
s1 xs@(_:xxs) = length $ filter (==True) $ zipWith (>) xxs xs

s2 :: [Int] -> Int
s2 = s1 . triSums

triSums :: [Int] -> [Int]
triSums xs@(_:xxs) = zipWith3 (sum3) xs xxs xxxs
    where
        xxxs = tail xxs
        sum3 x y z = x + y + z

input :: [Int]
input = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
