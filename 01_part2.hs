countIncreases :: [Integer] -> Int
countIncreases nums = length $ filter increases $ zip sums (tail sums)
        where increases (x, y) = y > x
              threeMeasurementWindows = zip3 nums (tail nums) (tail (tail nums))
              sums = map (\(x, y, z) -> x + y + z) threeMeasurementWindows

main = do
        input <- getContents
        let nums = map read $ lines input
        putStrLn $ show $ countIncreases nums
