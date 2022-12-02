module Day10 where

-- Needed imports
import Data.Maybe (mapMaybe)
import Data.List (sort)

-- Do not change. The main program will access the solutions from this function
solve :: String -> String
solve fcontent = "Solution 1:\t" ++ sol1 ++ "\nSolution 2:\t" ++ sol2 ++ "\n"
    where problemInput = parseFileContent fcontent
          sol1 = (show . s1) problemInput
          sol2 = (show . s2) problemInput

------------------------------------------------------------------------------
-- Change according to the problem
type ProblemInput = [String]
type Stack a = [a]

-- Parse the input file
parseFileContent :: String -> ProblemInput
parseFileContent = lines

isOpening :: Char -> Bool
isOpening c = c `elem` "(<[{"

illegalScore :: Char -> Int
illegalScore ')' = 3
illegalScore ']' = 57
illegalScore '}' = 1197
illegalScore '>' = 25137
illegalScore c = error $ "No suitable illegal score found for character " ++ [c]

complement :: Char -> Char
complement '(' = ')'
complement '<' = '>'
complement '[' = ']'
complement '{' = '}'
complement c = error $ "No complement found for character " ++ [c]

illegalCharacter :: Stack Char -> String -> Maybe Char
illegalCharacter _ [] = Nothing
illegalCharacter ss (x:xs)
  | isOpening x               = illegalCharacter (x:ss) xs
  | complement (head ss) == x = illegalCharacter (tail ss) xs
  | otherwise                 = Just x

-- Solve the first part
s1 :: ProblemInput -> Int
s1 = sum . mapMaybe (fmap illegalScore . illegalCharacter [])

remainingChars :: Stack Char -> String -> Stack Char
remainingChars ss [] = map complement ss
remainingChars ss (x:xs)
  | isOpening x               = remainingChars (x:ss) xs
  | complement (head ss) == x = remainingChars (tail ss) xs
remainingChars _ _ = []

incompleteScore :: Char -> Int
incompleteScore ')' = 1
incompleteScore ']' = 2
incompleteScore '}' = 3
incompleteScore '>' = 4
incompleteScore c = error $ "No suitable incomplete score found for character " ++ [c]

completionScore :: String -> Int
completionScore = foldl (\a x-> a * 5 + incompleteScore x) 0

-- Solve the second part
s2 :: ProblemInput -> Int
s2 pi = (!! middle) . sort . map completionScore $ completionList
    where middle = length completionList `div` 2
          completionList = filter (not . null) $ map (remainingChars []) pi
