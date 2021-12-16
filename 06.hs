parseInput :: String -> [Integer]
parseInput str = read ("[" ++ str ++ "]")

runCycle :: [Integer] -> [Integer]
runCycle [] = []
runCycle (x:xs) | x == 0 = (6:8:(runCycle xs))
                | otherwise = (x-1:(runCycle xs))

afterCycles :: [Integer] -> Integer -> [Integer]
afterCycles xs 0 = xs
afterCycles xs n = afterCycles (runCycle xs) (n - 1)

main = do
        input <- getContents
        let timers = parseInput input
        print $ length $ afterCycles timers 80
        print $ length $ afterCycles timers 256
