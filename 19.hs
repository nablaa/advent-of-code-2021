import Data.List
import Data.Maybe
import Control.Monad
import qualified Data.Set as S

type Matrix = [[Int]]
type Coordinates = (Int, Int, Int)
data Scanner = Scanner {
        detections :: [Coordinates],
        position :: Maybe Coordinates
} deriving (Show, Eq)

parseInput :: String -> [Scanner]
parseInput = parseScanners . lines

parseScanners :: [String] -> [Scanner]
parseScanners [] = []
parseScanners ls = scanner : parseScanners rest
        where (scanner, rest) = parseScanner ls

parseScanner :: [String] -> (Scanner, [String])
parseScanner ls = (Scanner detections Nothing, rest)
        where detectionLines = takeWhile (/= "") $ tail ls
              rest = dropWhile (== "") $ dropWhile (/= "") $ tail ls
              detections = map (read . (\s -> "(" ++ s ++ ")")) detectionLines

scannerRotations :: Scanner -> [Scanner]
scannerRotations (Scanner detections p) = map (`Scanner` p) rots
        where rots = rotations detections

rotations :: [Coordinates] -> [[Coordinates]]
rotations ds = map f rotationMatrices
        where f rot = map (`rotate` rot) ds

rotate :: Coordinates -> Matrix -> Coordinates
rotate (x, y, z) [[a11, a12, a13], [a21, a22, a23], [a31, a32, a33]] = (nx, ny, nz)
        where nx = a11 * x + a12 * y + a13 * z
              ny = a21 * x + a22 * y + a23 * z
              nz = a31 * x + a32 * y + a33 * z
rotate _ _ = undefined

setInitialScanner :: [Scanner] -> [Scanner]
setInitialScanner scanners = scanner : tail scanners
        where (Scanner ds _) = head scanners
              scanner = Scanner ds (Just (0, 0, 0))

beaconCount :: [Scanner] -> Int
beaconCount = length . nub . sort . concatMap detections . connectAllScanners

connectAllScanners :: [Scanner] -> [Scanner]
connectAllScanners scanners = connectAllScanners' rest [first]
        where scanners' = setInitialScanner scanners
              first = head scanners'
              rest = tail scanners'

connectAllScanners' :: [Scanner] -> [Scanner] -> [Scanner]
connectAllScanners' [] connected = connected
connectAllScanners' (x:xs) connected = case connect x connected of
                                         Just s -> connectAllScanners' xs (s:connected)
                                         Nothing -> connectAllScanners' (xs ++ [x]) connected

connect :: Scanner -> [Scanner] -> Maybe Scanner
connect unconnected [] = Nothing
connect unconnected (x:xs) = case rotationAndTranslationToConnect x unconnected of
                               Just (translation, rotation) -> Just $ applyRotationTranslation unconnected translation rotation
                               Nothing -> connect unconnected xs

applyRotationTranslation :: Scanner -> Coordinates -> Matrix -> Scanner
applyRotationTranslation (Scanner _ (Just _)) _ _ = error "already applied"
applyRotationTranslation (Scanner detection Nothing) translation rotation = Scanner (map (translate translation . (`rotate` rotation)) detection) (Just translation)

rotationAndTranslationToConnect :: Scanner -> Scanner -> Maybe (Coordinates, Matrix)
rotationAndTranslationToConnect (Scanner _ Nothing) _ = error "not connected"
rotationAndTranslationToConnect _ (Scanner _ (Just _)) = error "already connected"
rotationAndTranslationToConnect (Scanner detections (Just _)) (Scanner ds Nothing) = if null m2 then Nothing else Just $ head m2
        where rotatedPoints = [(matrix, map (`rotate` matrix) ds) | matrix <- rotationMatrices]
              rotAndTra = [(matrix, findMatchingTranslation detections points) | (matrix, points) <- rotatedPoints]
              m2 = map (\(mat, Just tr) -> (tr, mat)) $ filter (\(_, mb) -> isJust mb) rotAndTra

findMatchingTranslation :: [Coordinates] -> [Coordinates] -> Maybe Coordinates
findMatchingTranslation fixed points = msum matching
        where translations = possibleTranslations fixed points
              matching = map (matchingTranslation fixed points) translations

matchingTranslation :: [Coordinates] -> [Coordinates] -> Coordinates -> Maybe Coordinates
matchingTranslation fixed points translation = if matches then Just translation else Nothing
        where translatedPoints = map (translate translation) points
              matches = translationMatches fixed translatedPoints


possibleTranslations :: [Coordinates] -> [Coordinates] -> [Coordinates]
possibleTranslations points1 points2 = concat [[subtractCoord p1 p2 | p2 <- points2] | p1 <- points1]

translationMatches :: [Coordinates] -> [Coordinates] -> Bool
translationMatches fixed points = S.size (S.intersection (S.fromList fixed) (S.fromList points)) >= 12

translate :: Coordinates -> Coordinates -> Coordinates
translate (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

subtractCoord :: Coordinates -> Coordinates -> Coordinates
subtractCoord (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

manhattan :: Coordinates -> Coordinates -> Int
manhattan (x1, y1, z1) (x2, y2, z2) = abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)

rotationMatrices :: [Matrix]
rotationMatrices = [
                        [
                                [1, 0, 0],
                                [0, 1, 0],
                                [0, 0, 1]
                        ],
                        [
                                [1, 0, 0],
                                [0, 0, -1],
                                [0, 1, 0]
                        ],
                        [
                                [1, 0, 0],
                                [0, -1, 0],
                                [0, 0, -1]
                        ],
                        [
                                [1, 0, 0],
                                [0, 0, 1],
                                [0, -1, 0]
                        ],
                        [
                                [0, -1, 0],
                                [1, 0, 0],
                                [0, 0, 1]
                        ],
                        [
                                [0, 0, 1],
                                [1, 0, 0],
                                [0, 1, 0]
                        ],
                        [
                                [0, 1, 0],
                                [1, 0, 0],
                                [0, 0, -1]
                        ],
                        [
                                [0, 0, -1],
                                [1, 0, 0],
                                [0, -1, 0]
                        ],
                        [
                                [-1, 0, 0],
                                [0, -1, 0],
                                [0, 0, 1]
                        ],
                        [
                                [-1, 0, 0],
                                [0, 0, -1],
                                [0, -1, 0]
                        ],
                        [
                                [-1, 0, 0],
                                [0, 1, 0],
                                [0, 0, -1]
                        ],
                        [
                                [-1, 0, 0],
                                [0, 0, 1],
                                [0, 1, 0]
                        ],
                        [
                                [0, 1, 0],
                                [-1, 0, 0],
                                [0, 0, 1]
                        ],
                        [
                                [0, 0, 1],
                                [-1, 0, 0],
                                [0, -1, 0]
                        ],
                        [
                                [0, -1, 0],
                                [-1, 0, 0],
                                [0, 0, -1]
                        ],
                        [
                                [0, 0, -1],
                                [-1, 0, 0],
                                [0, 1, 0]
                        ],
                        [
                                [0, 0, -1],
                                [0, 1, 0],
                                [1, 0, 0]
                        ],
                        [
                                [0, 1, 0],
                                [0, 0, 1],
                                [1, 0, 0]
                        ],
                        [
                                [0, 0, 1],
                                [0, -1, 0],
                                [1, 0, 0]
                        ],
                        [
                                [0, -1, 0],
                                [0, 0, -1],
                                [1, 0, 0]
                        ],
                        [
                                [0, 0, -1],
                                [0, -1, 0],
                                [-1, 0, 0]
                        ],
                        [
                                [0, -1, 0],
                                [0, 0, 1],
                                [-1, 0, 0]
                        ],
                        [
                                [0, 0, 1],
                                [0, 1, 0],
                                [-1, 0, 0]
                        ],
                        [
                                [0, 1, 0],
                                [0, 0, -1],
                                [-1, 0, 0]
                        ]
                   ]


main = do input <- getContents
          let scanners = parseInput input
          print $ beaconCount scanners
