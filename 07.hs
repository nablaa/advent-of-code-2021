import Data.List

median :: [Int] -> Int
median nums | even (length nums) = sorted !! (length nums `div` 2)
            | otherwise = (sorted !! n1 + sorted !! n2) `div` 2
        where sorted = sort nums
              n1 = (length nums - 1) `div` 2
              n2 = (length nums + 1) `div` 2

simpleCost :: [Int] -> Int
simpleCost nums = sum $ map (\n -> abs (med - n)) nums
        where med = median nums

costToPosition :: Int -> Int -> Int
costToPosition to from = distance * (distance + 1) `div` 2
        where distance = abs (from - to)

costsTo :: [Int] -> Int -> Int
costsTo nums pos = sum $ map (costToPosition pos) nums

lowestCost :: [Int] -> Int
lowestCost nums = minimum $ map (costsTo nums) [smallest..largest]
        where smallest = minimum nums
              largest = maximum nums

parseInput :: String -> [Int]
parseInput str = read ("[" ++ str ++ "]")

main = do
        input <- getContents
        let nums = parseInput input
        print $ simpleCost $ nums
        print $ lowestCost $ nums
