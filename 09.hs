import Data.Array
import Data.List

type Coordinates = (Int, Int)
type Heightmap = Array Coordinates Integer
type Basin = [Coordinates]

parseInput :: String -> [[Integer]]
parseInput = map parseLine . lines

parseLine :: String -> [Integer]
parseLine = map (\c -> read [c])

mapSize :: [[Integer]] -> Coordinates
mapSize nums = (length nums - 1, length (transpose nums) - 1)

heightmap :: [[Integer]] -> Heightmap
heightmap nums = listArray ((0, 0), mapSize nums) (concat nums)

neighbors :: Heightmap -> Coordinates -> [Coordinates]
neighbors hmap (x, y) = filter withinBounds [left, right, up, down]
        where left = (x - 1, y)
              right = (x + 1, y)
              up = (x, y - 1)
              down = (x, y + 1)
              (xMax, yMax) = snd $ bounds hmap
              withinBounds (i, j) = i >= 0 && i <= xMax && j >= 0 && j <= yMax

height :: Heightmap -> Coordinates -> Integer
height = (!)

lowerThanNeighbor :: Heightmap -> Coordinates -> Bool
lowerThanNeighbor hmap coords = all (cellHeight <) neighborHeights
        where neighborHeights = map (height hmap) $ neighbors hmap coords
              cellHeight = height hmap coords

cellsLowerThanNeighbor :: Heightmap -> [Coordinates]
cellsLowerThanNeighbor hmap = filter (lowerThanNeighbor hmap) allCells
        where allCells = indices hmap

riskLevel :: Heightmap -> Coordinates -> Integer
riskLevel hmap coords = 1 + height hmap coords

sumRisk :: Heightmap -> Integer
sumRisk hmap = sum $ map (riskLevel hmap) lowerCells
        where lowerCells = cellsLowerThanNeighbor hmap

basins :: Heightmap -> [Basin]
basins hmap = map (basin hmap) startCells
        where startCells = cellsLowerThanNeighbor hmap

basin :: Heightmap -> Coordinates -> Basin
basin hmap coords = nub $ sort $ floodFill hmap [coords] []

floodFill :: Heightmap -> [Coordinates] -> [Coordinates] -> [Coordinates]
floodFill _ [] _ = []
floodFill hmap (x:xs) visited | withinBasin hmap x = x : floodFill hmap newQueue (x:visited)
                              | otherwise = floodFill hmap xs (x:visited)
        where neighborCells = neighbors hmap x
              addedCells = filter notVisited neighborCells
              newQueue = xs ++ addedCells
              notVisited c = c `notElem` visited

withinBasin :: Heightmap -> Coordinates -> Bool
withinBasin hmap coords = height hmap coords < 9

basinSizes :: Heightmap -> [Int]
basinSizes hmap = map length (basins hmap)

largestBasinSizes :: Heightmap -> Int -> [Int]
largestBasinSizes hmap n = take n (reverse (sort (basinSizes hmap)))

main = do
        input <- getContents
        let nums = parseInput input
            hmap = heightmap nums
        print $ sumRisk hmap
        print $ product (largestBasinSizes hmap 3)
