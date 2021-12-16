parseInput :: String -> [Integer]
parseInput = map read . lines

countIncreases :: [Integer] -> Int
countIncreases nums = length $ filter increases $ zip nums (tail nums)
        where increases (x, y) = y > x

main = do
        input <- getContents
        let nums = parseInput input
        putStrLn $ show $ countIncreases nums
