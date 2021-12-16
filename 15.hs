import Data.Array
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

type Coordinates = (Int, Int)
type Grid = Array Coordinates Integer

parseInput :: String -> [[Integer]]
parseInput = map parseLine . lines

parseLine :: String -> [Integer]
parseLine = map (\c -> read [c])

mapSize :: [[Integer]] -> Coordinates
mapSize nums = (length nums - 1, length (transpose nums) - 1)

makeGrid :: [[Integer]] -> Grid
makeGrid nums = listArray ((0, 0), mapSize nums) (concat nums)

neighbors :: Grid -> Coordinates -> [Coordinates]
neighbors hmap (x, y) = filter withinBounds [left, right, up, down]
        where left = (x - 1, y)
              right = (x + 1, y)
              up = (x, y - 1)
              down = (x, y + 1)
              (xMax, yMax) = snd $ bounds hmap
              withinBounds (i, j) = i >= 0 && i <= xMax && j >= 0 && j <= yMax

level :: Grid -> Coordinates -> Integer
level = (!)

astar :: Grid -> Coordinates -> Coordinates -> (Coordinates -> Coordinates -> Integer) -> [Coordinates]
astar grid start end h = astar' grid start end h openSet cameFrom gScore fScore
        where openSet = S.insert start S.empty
              cameFrom = M.empty
              gScore = M.insert start 0 M.empty
              fScore = M.insert start (h start end) M.empty

astar' :: Grid -> Coordinates -> Coordinates -> (Coordinates -> Coordinates -> Integer)
        -> S.Set Coordinates -> M.Map Coordinates Coordinates -> M.Map Coordinates Integer -> M.Map Coordinates Integer -> [Coordinates]
astar' grid start end h openSet cameFrom gScore fScore | S.size openSet == 0 = error "empty openSet"
                                                       | current == end = delete start (reconstructPath cameFrom current)
                                                       | otherwise = astar' grid start end h newOpenSet newCameFrom newgScore newfScore
        where current = snd $ minimum $ map (\c -> (nodeFScore fScore c, c)) (S.elems openSet)
              newSet = S.delete current openSet
              neighs = neighbors grid current
              (newCameFrom, newgScore, newfScore, newOpenSet) = neighborPaths grid current newSet cameFrom gScore fScore neighs (h end)

neighborPaths :: Grid -> Coordinates -> S.Set Coordinates -> M.Map Coordinates Coordinates -> M.Map Coordinates Integer -> M.Map Coordinates Integer -> [Coordinates]
              -> (Coordinates -> Integer)
              -> (M.Map Coordinates Coordinates, M.Map Coordinates Integer, M.Map Coordinates Integer, S.Set Coordinates)
neighborPaths _ _ openSet cameFrom gScore fScore [] _ = (cameFrom, gScore, fScore, openSet)
neighborPaths grid current openSet cameFrom gScore fScore (neigh:gs) h | tentativeG >= nodeGScore gScore neigh = neighborPaths grid current openSet cameFrom gScore fScore gs h
                                                                       | otherwise = neighborPaths grid current newOpenSet newCameFrom newGscore newFscore gs h
        where tentativeG = tentativeGScore grid gScore current neigh
              newCameFrom = M.insert neigh current cameFrom
              newGscore = M.insert neigh tentativeG gScore
              newFscore = M.insert neigh (tentativeG + h neigh) fScore
              newOpenSet = S.insert neigh openSet

nodeFScore :: M.Map Coordinates Integer -> Coordinates -> Integer
nodeFScore fScore node = M.findWithDefault 9999999999 node fScore

nodeGScore :: M.Map Coordinates Integer -> Coordinates -> Integer
nodeGScore gScore node = M.findWithDefault 9999999999 node gScore

tentativeGScore :: Grid -> M.Map Coordinates Integer -> Coordinates -> Coordinates -> Integer
tentativeGScore grid gScore current neigh = nodeGScore gScore current + level grid neigh

distance :: Coordinates -> Coordinates -> Integer
distance (x1, y1) (x2, y2) = fromIntegral $ abs (x2 - x1) + abs (y2 - y1)

reconstructPath :: M.Map Coordinates Coordinates -> Coordinates -> [Coordinates]
reconstructPath cameFrom current = current : reconstructPath' cameFrom current []

reconstructPath' :: M.Map Coordinates Coordinates -> Coordinates -> [Coordinates]-> [Coordinates]
reconstructPath' cameFrom current totalPath | isNothing (M.lookup current cameFrom) = totalPath
                                            | otherwise = current' : reconstructPath' newCameFrom current' totalPath
        where current' = cameFrom M.! current
              newCameFrom = M.delete current cameFrom

pathRisk :: Grid -> [Coordinates] -> Integer
pathRisk grid cs = sum $ map (level grid) cs

main = do
        input <- getContents
        let nums = parseInput input
            grid = makeGrid nums
            path = reverse $ astar grid (0, 0) (snd (bounds grid)) distance
        print $ path
        print $ map (level grid) $ path
        print $ pathRisk grid $ path
