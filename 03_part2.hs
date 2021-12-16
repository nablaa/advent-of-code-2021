import Data.List

parseInput :: String -> [[Bool]]
parseInput = map parseBits . lines

parseBits :: String -> [Bool]
parseBits = map toBit
        where toBit '0' = False
              toBit '1' = True

powerConsumption :: [[Bool]] -> Integer
powerConsumption numbers = gammaRate numbers * epsilonRate numbers

gammaRate :: [[Bool]] -> Integer
gammaRate = bitsToNumber . map mostCommon . transpose

epsilonRate :: [[Bool]] -> Integer
epsilonRate = bitsToNumber . map leastCommon . transpose

mostCommon :: [Bool] -> Bool
mostCommon bits = if f > t then False else True
        where [f, t] = countBits bits

leastCommon :: [Bool] -> Bool
leastCommon bits = if f <= t then False else True
        where [f, t] = countBits bits

countBits :: [Bool] -> [Int]
countBits = map length . groupBy (==) . sort

bitsToNumber :: [Bool] -> Integer
bitsToNumber = sum . reverse . zipWith (\n b -> if b then n else 0) (binaryValues) . reverse
        where binaryValues = [2^n | n <- [0..]]

lifeSupportRating :: [[Bool]] -> Integer
lifeSupportRating numbers = oxygenGeneratorRating numbers * co2ScrubberRating numbers

oxygenGeneratorRating :: [[Bool]] -> Integer
oxygenGeneratorRating = bitsToNumber . runFilter mostCommon

co2ScrubberRating :: [[Bool]] -> Integer
co2ScrubberRating = bitsToNumber . runFilter leastCommon

runFilter :: ([Bool] -> Bool) -> [[Bool]] -> [Bool]
runFilter criteria numbers = runFilter' criteria numbers 0

runFilter' :: ([Bool] -> Bool) -> [[Bool]] -> Int -> [Bool]
runFilter' criteria [x] n = x
runFilter' criteria numbers n = runFilter' criteria (filterByBitCriteria criteria numbers n) (n + 1)

filterByBitCriteria :: ([Bool] -> Bool) -> [[Bool]] -> Int -> [[Bool]]
filterByBitCriteria criteria numbers index = filter f numbers
        where bit = (map criteria $ transpose numbers) !! index
              f n = (n !! index) == bit


main = do
        input <- getContents
        let numbers = parseInput input
        putStrLn $ show $ powerConsumption numbers
        putStrLn $ show $ lifeSupportRating numbers
