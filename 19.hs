import Data.List
import Control.Monad
import qualified Data.Set as S

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

rotate :: Coordinates -> [[Int]] -> Coordinates
rotate (x, y, z) [[a11, a12, a13], [a21, a22, a23], [a31, a32, a33]] = (nx, ny, nz)
        where nx = a11 * x + a12 * y + a13 * z
              ny = a21 * x + a22 * y + a23 * z
              nz = a31 * x + a32 * y + a33 * z
rotate _ _ = undefined

setInitialScanner :: [Scanner] -> [Scanner]
setInitialScanner scanners = scanner : tail scanners
        where (Scanner ds _) = head scanners
              scanner = Scanner ds (Just (0, 0, 0))

findConnectingScanner :: Scanner -> [Scanner] -> [Scanner]
findConnectingScanner scanner [] = error "no more scanners to check"
findConnectingScanner fixed (scanner:scanners) = case scannerConnected fixed scanner of
                                                   Just s -> s : scanners
                                                   Nothing -> scanner : findConnectingScanner fixed scanners

scannerConnected :: Scanner -> Scanner -> Maybe Scanner
scannerConnected (Scanner detections _) (Scanner ds (Just _)) = error "already connected"
scannerConnected (Scanner detections _) (Scanner ds Nothing) = case matching of
                                                                   Just translation -> Just $ Scanner (translatedPoints translation) (Just translation)
                                                                   Nothing -> Nothing
        where allRotations = rotations ds
              rotationsMatching = map (findMatchingTranslation detections) allRotations
              matching = msum rotationsMatching
              translatedPoints t = map (translate t) ds


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

rotationMatrices :: [[[Int]]]
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
          print scanners
