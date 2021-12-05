import Data.Array
import Data.List

type Coordinates = (Int, Int)

line :: Coordinates -> Coordinates -> [Coordinates]
line c1@(x1, y1) c2@(x2, y2) | x1 == x2 = if y1 < y2 then verticalLine c1 c2 else verticalLine c2 c1
                             | y1 == y2 = if x1 < x2 then horizontalLine c1 c2 else horizontalLine c2 c1
                             | otherwise = if x1 < x2 then diagonalLine c1 c2 else diagonalLine c2 c1

verticalLine :: Coordinates -> Coordinates -> [Coordinates]
verticalLine (x1, y1) (x2, y2) = [(x1, y) | y <- [y1..y2]]

horizontalLine :: Coordinates -> Coordinates -> [Coordinates]
horizontalLine (x1, y1) (x2, y2) = [(x, y1) | x <- [x1..x2]]

diagonalLine :: Coordinates -> Coordinates -> [Coordinates]
diagonalLine (x1, y1) (x2, y2) | y1 < y2 = [(x1 + step, y1 + step) | step <- [0..(y2-y1)]]
                               | otherwise = [(x1 + step, y1 - step) | step <- [0..(y1-y2)]]

parseInput :: String -> [(Coordinates, Coordinates)]
parseInput = map parseEndpoints . lines

parseEndpoints :: String -> (Coordinates, Coordinates)
parseEndpoints str = (read c1, read c2)
        where w = words str
              c1 = "(" ++ head w ++ ")"
              c2 = "(" ++ last w ++ ")"

maxCoordinate :: [Coordinates] -> Coordinates
maxCoordinate cs = maxCoordinate' cs (0,0)

maxCoordinate' :: [Coordinates] -> Coordinates -> Coordinates
maxCoordinate' [] c = c
maxCoordinate' ((x,y):cs) (mx,my) = maxCoordinate' cs (max mx x, max my y)

overlappingCounts :: [Coordinates] -> [Int]
overlappingCounts = map length . group . sort


main = do input <- getContents
          let endpoints = parseInput input
              ls = map (uncurry line) endpoints
              overlapping = overlappingCounts (concat ls)
              atLeast2 = filter (>= 2) overlapping
          print $ length atLeast2
