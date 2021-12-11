import Data.Array
import Data.List

type Coordinates = (Int, Int)
type Grid = Array Coordinates Cell
data Cell = Cell Integer Bool deriving (Show, Eq)

parseInput :: String -> [[Integer]]
parseInput = map parseLine . lines
        where parseLine = map (\c -> read [c])

gridSize :: [[Integer]] -> Coordinates
gridSize nums = (length nums - 1, length (transpose nums) - 1)

makeGrid :: [[Integer]] -> Grid
makeGrid nums = listArray ((0, 0), gridSize nums) (map toCell $ concat nums)
        where toCell n = Cell n False

printGrid :: Grid -> String
printGrid grid = toLines $ foldr f "" (elems grid)
    where f = (:) . printCell
          lineLength = 1 + snd (snd $ bounds grid)
          toLines [] = []
          toLines str = take lineLength str ++ "\n" ++ toLines (drop lineLength str)
          printCell (Cell n _) | n < 10 = head $ show n
                               | otherwise = 'X'

neighbors :: Grid -> Coordinates -> [Coordinates]
neighbors grid (x, y) = filter withinBounds [left, right, up, down, upLeft, upRight, downLeft, downRight]
        where left = (x - 1, y)
              right = (x + 1, y)
              up = (x, y - 1)
              down = (x, y + 1)
              upLeft = (x - 1, y - 1)
              upRight = (x + 1, y - 1)
              downLeft = (x - 1, y - 1)
              downRight = (x - 1, y + 1)
              (xMax, yMax) = snd $ bounds grid
              withinBounds (i, j) = i >= 0 && i <= xMax && j >= 0 && j <= yMax


generateSteps :: Grid -> [(Integer, Grid)]
generateSteps grid = result : generateSteps newGrid
        where result@(flashes, newGrid) = runStep grid

runNSteps :: Grid -> Integer -> (Integer, Grid)
runNSteps grid n | n == 1 = runStep grid
                 | otherwise = (flashes + nflashes, ngrid)
        where (flashes, newGrid) = runStep grid
              (nflashes, ngrid) = runNSteps newGrid (n - 1)

runStep :: Grid -> (Integer, Grid)
runStep grid = runFlashes increasedGrid (indices increasedGrid) 0
        where increasedGrid = fmap (\(Cell n b) -> Cell (n + 1) b) grid

runFlashes :: Grid -> [Coordinates] -> Integer -> (Integer, Grid)
runFlashes grid cs flashes | isAnyOver9 grid = runFlashes newGrid cs newFlashes
                           | otherwise = (flashes, grid)
        where (newFlashes, newGrid) = runFlashes' grid cs flashes

isAnyOver9 :: Grid -> Bool
isAnyOver9 grid = any (\(Cell n _) -> n > 9) (elems grid)

runFlashes' :: Grid -> [Coordinates] -> Integer -> (Integer, Grid)
runFlashes' grid [] flashes = (flashes, grid)
runFlashes' grid (c@(x, y):cs) flashes | shouldFlash grid c = runFlashes' g10 cs (flashes + 1)
                                       | otherwise = runFlashes' grid cs flashes
        where g1 = flashCell grid c
              g2 = increaseCell g1 (x - 1, y - 1)
              g3 = increaseCell g2 (x, y - 1)
              g4 = increaseCell g3 (x + 1, y - 1)
              g5 = increaseCell g4 (x - 1, y)
              g6 = increaseCell g5 (x, y)
              g7 = increaseCell g6 (x + 1, y)
              g8 = increaseCell g7 (x - 1, y + 1)
              g9 = increaseCell g8 (x, y + 1)
              g10 = increaseCell g9 (x + 1, y + 1)

increaseCell :: Grid -> Coordinates -> Grid
increaseCell grid c@(x, y) | withinBounds = grid // [(c, inc (grid ! c))]
                           | otherwise = grid
        where (xMax, yMax) = snd $ bounds grid
              withinBounds = x >= 0 && x <= xMax && y >= 0 && y <= yMax
              inc (Cell 0 b) = Cell 0 b
              inc (Cell n b) = Cell (n + 1) b

shouldFlash :: Grid -> Coordinates -> Bool
shouldFlash grid coords = n > 9
        where (Cell n b) = grid ! coords

flashCell :: Grid -> Coordinates -> Grid
flashCell grid coords = newGrid
        where (Cell n b) = grid ! coords
              newCell = Cell 0 True
              newGrid = grid // [(coords, newCell)]

allZeros :: Grid -> Bool
allZeros grid = all (\(Cell n _) -> n == 0) (elems grid)

firstStepWithAllZeros :: Grid -> Int
firstStepWithAllZeros grid = 1 + length (takeWhile (\(_, g) -> not (allZeros g)) allSteps)
        where allSteps = generateSteps grid


main = do input <- getContents
          let grid = makeGrid $ parseInput input
              (flashes, endGrid) = runNSteps grid 100
          --putStrLn $ printGrid endGrid
          print flashes
          print $ firstStepWithAllZeros grid
