import Data.Array
import Data.List
import qualified Data.Set as S
import qualified Data.Map.Strict as M

type Bits = [Bool]
type Coordinates = (Int, Int)
type Image = Array Coordinates Bool
type PixelLookup = M.Map Int Bool

parseInput :: String -> (PixelLookup, Image)
parseInput input = (parsePixelLookup (head ls), parseImage $ addExtraRow $ drop 2 ls)
        where ls = lines input

parsePixelLookup :: String -> PixelLookup
parsePixelLookup line = M.fromList lookups
        where lookups = zip [0..] (map toBool line)

addExtraRow :: [String] -> [String]
addExtraRow ls = newRow : ls
        where newRow = replicate (length $ head ls) '.'

parseImage :: [String] -> Image
parseImage ls = listArray ((0, 0), (ysize - 1, xsize - 1)) elems
        where xsize = length $ transpose ls
              ysize = length ls
              elems = map toBool $ concat ls

printImage :: Image -> String
printImage image = printImage' (elems image)
        where ((minY, minX), (maxY, maxX)) = bounds image
              xsize = maxX - minX + 1
              printImage' [] = ""
              printImage' pixels = map toChar (take xsize pixels) ++ "\n" ++ printImage' (drop xsize pixels)

toChar :: Bool -> Char
toChar False = '.'
toChar True = '#'

toBool :: Char -> Bool
toBool '.' = False
toBool '#' = True
toBool _ = undefined

expandImage :: Image -> Image
expandImage image = newImage
        where ((minY, minX), (maxY, maxX)) = bounds image
              newBounds = ((minY - 3, minX - 3), (maxY + 3, maxX + 3))
              ((newMinY, newMinX), (newMaxY, newMaxX)) = newBounds
              newImage = array newBounds newPixels
              newPixels = assocs image ++ additionalPixels
              allIndices = S.fromList $ concat [[(y, x) | x <- [newMinX..newMaxX]] | y <- [newMinY..newMaxY]]
              newIndices = S.difference allIndices (S.fromList $ indices image)
              additionalPixels = zip (S.toList newIndices) (repeat (backgroundPixel image))

runConvolution :: PixelLookup -> Image -> Image
runConvolution pixelLookup image = newImage
        where newPixels = map (\index -> (index, convolute pixelLookup image index)) (indices image)
              newImage = array (bounds image) newPixels

convolute :: PixelLookup -> Image -> Coordinates -> Bool
convolute pixelLookup image coords = pixel
        where index = bitsToDecimal $ squarePixels image coords
              pixel = pixelLookup M.! index

squarePixels :: Image -> Coordinates -> Bits
squarePixels image (y, x) = map (pixel image) cs
        where cs = concat [[(y + i, x + j) | j <- [-1..1]] | i <- [-1..1]]

pixel :: Image -> Coordinates -> Bool
pixel image c@(y, x) | withinBounds = image ! c
                     | otherwise = backgroundPixel image
        where ((minY, minX), (maxY, maxX)) = bounds image
              withinBounds = x >= minX && x <= maxX && y >= minY && y <= maxY

bitsToDecimal :: Bits -> Int
bitsToDecimal = sum . reverse . zipWith (\n b -> if b then n else 0) binaryValues . reverse
        where binaryValues = [2^n | n <- [0..]]

backgroundPixel :: Image -> Bool
backgroundPixel image = image ! fst (bounds image)

litPixels :: Image -> Int
litPixels = length . filter id . elems

runEnhancement :: PixelLookup -> Image -> Int -> Image
runEnhancement _ image 0 = image
runEnhancement pixelLookup image n = runEnhancement pixelLookup newImage (n - 1)
        where newImage = runConvolution pixelLookup (expandImage image)

main = do input <- getContents
          let (pixelLookup, image) = parseInput input
          print $ litPixels $ runEnhancement pixelLookup image 2
