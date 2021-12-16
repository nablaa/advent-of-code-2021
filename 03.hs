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
leastCommon bits = if f < t then False else True
        where [f, t] = countBits bits

countBits :: [Bool] -> [Int]
countBits = map length . groupBy (==) . sort

bitsToNumber :: [Bool] -> Integer
bitsToNumber = sum . reverse . zipWith (\n b -> if b then n else 0) (binaryValues) . reverse
        where binaryValues = [2^n | n <- [0..]]


main = do
        input <- getContents
        let numbers = parseInput input
        putStrLn $ show $ powerConsumption numbers
