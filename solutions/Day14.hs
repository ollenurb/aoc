module Day14 where

-- Needed imports
import Data.HashMap.Lazy (HashMap, fromList, (!), empty, insertWith, elems)

-- Do not change. The main program will access the solutions from this function
solve :: String -> String
solve fcontent = "Solution 1:\t" ++ sol1 ++ "\nSolution2:\t" ++ sol2 ++ "\n"
    where problemInput = parseFileContent fcontent
          sol1 = (show . s1) problemInput
          sol2 = (show . s1) problemInput

------------------------------------------------------------------------------
-- Change according to the problem
type ProblemInput = (Rules, String)
type Rules = HashMap String Char
type Histogram a = HashMap a Int

testRules = fromList [("CH", 'B'),
                      ("HH",'N'),
                      ("CB",'H'),
                      ("NH",'C'),
                      ("HB",'C'),
                      ("HC",'B'),
                      ("HN",'C'),
                      ("NN",'C'),
                      ("BH",'H'),
                      ("NC",'B'),
                      ("NB",'B'),
                      ("BN",'B'),
                      ("BB",'N'),
                      ("BC",'B'),
                      ("CC",'N'),
                      ("CN",'C')]


step :: Rules -> String -> String
step rules str@(s:ss) = s : intercalated
    where rs = zipWith (\x y -> rules ! [x, y]) str ss
          intercalated = concat $ zipWith (\x y -> [y, x]) ss rs

stepN :: Rules -> Int -> String -> String
stepN rules n = (!! n) . iterate (step rules)

lcmc :: String -> (Int, Int)
lcmc s = (maximum hl, minimum hl)
    where hl = elems $ computeHistogram s
          computeHistogram = foldr (flip (insertWith (+)) 1) empty

-- Parse the input file
parseFileContent :: String -> ProblemInput
parseFileContent fc = (rulesMap, template)
    where (template:"":rules) = lines fc
          rulesMap = fromList $ map parseRule rules

parseRule :: String -> (String, Char)
parseRule s = (k, head v)
    where [k, "->", v] = words s

-- Solve the first part
s1 :: ProblemInput -> Int
s1 (rules, template) = uncurry (-) $ lcmc $ stepN rules 10 template

-- Solve the second part
s2 :: ProblemInput -> Int
s2 (rules, template) = uncurry (-) $ lcmc $ stepN rules 40 template
