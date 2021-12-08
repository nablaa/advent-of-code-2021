import Data.List

data Result = Ok | Incomplete [Char] | Corrupted Char Char
            deriving (Show, Eq)

parseLine :: String -> Result
parseLine str = parseLine' str []

parseLine' :: String -> [Char] -> Result
parseLine' [] [] = Ok
parseLine' [] expected = Incomplete expected
parseLine' (x:xs) [] | isOpening x = parseLine' xs [matchingClosing x]
                     | otherwise = undefined
parseLine' (x:xs) expected@(y:ys) | isOpening x = parseLine' xs (matchingClosing x : expected)
                                  | otherwise = if x == y then parseLine' xs ys
                                                          else Corrupted y x

isOpening :: Char -> Bool
isOpening '(' = True
isOpening '[' = True
isOpening '{' = True
isOpening '<' = True
isOpening _ = False

matchingClosing :: Char -> Char
matchingClosing '(' = ')'
matchingClosing '[' = ']'
matchingClosing '{' = '}'
matchingClosing '<' = '>'
matchingClosing _ = undefined

syntaxErrorScore :: [String] -> Integer
syntaxErrorScore = sum . map (score . parseLine)

score :: Result -> Integer
score (Corrupted _ ')') = 3
score (Corrupted _ ']') = 57
score (Corrupted _ '}') = 1197
score (Corrupted _ '>') = 25137
score _ = 0

isIncomplete :: Result -> Bool
isIncomplete (Incomplete _) = True
isIncomplete _ = False

incompleteLines :: [String] -> [String]
incompleteLines = filter (isIncomplete . parseLine)

completionValue :: Char -> Int
completionValue ')' = 1
completionValue ']' = 2
completionValue '}' = 3
completionValue '>' = 4
completionValue _ = 0

completionScore :: Result -> Int
completionScore (Incomplete chars) = charsScore chars
completionScore _ = 0

charsScore :: [Char] -> Int
charsScore = foldl addScore 0
        where addScore accum c = accum * 5 + completionValue c

completionScores :: [String] -> [Int]
completionScores = filter (0 /=) . map (completionScore . parseLine)

middleScore :: [Int] -> Int
middleScore scores = sort scores !! index
        where n = length scores - 1
              index = n `div` 2

main = do input <- getContents
          let strs = lines input
          print $ syntaxErrorScore strs
          print $ middleScore (completionScores strs)
