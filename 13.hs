import Data.List

data Fold = FoldUp Int | FoldLeft Int
        deriving (Show)

type Coordinates = (Int, Int)

parseInput :: String -> ([Coordinates], [Fold])
parseInput input = (map parseCoordinates coordinateLines, parseFolds foldLines)
        where ls = lines input
              coordinateLines = takeWhile (/= "") ls
              foldLines = dropWhile (== "") $ dropWhile (/= "") ls

parseCoordinates :: String -> Coordinates
parseCoordinates s = read ("(" ++ s ++ ")")

parseFolds :: [String] -> [Fold]
parseFolds = map (parseFold . last . words)

parseFold :: String -> Fold
parseFold s | xy == 'x' = FoldLeft num
            | xy == 'y' = FoldUp num
            | otherwise = undefined
        where xy = head s
              num = read (drop 2 s)

fold :: [Coordinates] -> Fold -> [Coordinates]
fold cs (FoldUp fy) = map (foldUp fy) cs
fold cs (FoldLeft fx) = map (foldLeft fx) cs

foldUp :: Int -> Coordinates -> Coordinates
foldUp fy (x, y) | y <= fy = (x, y)
                 | otherwise = (x, 2 * fy - y)

foldLeft :: Int -> Coordinates -> Coordinates
foldLeft fx (x, y) | x <= fx = (x, y)
                   | otherwise = (2 * fx - x, y)

printCoordinates :: [Coordinates] -> String
printCoordinates coords = unlines [ [toChar x y | x <- [0..mx] ] | y <- [0..my]]
        where (xs, ys) = unzip coords
              mx = maximum xs
              my = maximum ys
              toChar x y = if (x, y) `elem` coords then '#' else '.'

dotsAfterFirstFold :: [Coordinates] -> [Fold] -> Int
dotsAfterFirstFold coords folds = length $ nub $ sort $ fold coords (head folds)

runFolds :: [Coordinates] -> [Fold] -> [Coordinates]
runFolds = foldl fold

main = do input <- getContents
          let (dots, folds) = parseInput input
          print $ dotsAfterFirstFold dots folds
          putStrLn $ printCoordinates $ runFolds dots folds
