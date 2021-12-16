data Command = Forward Integer | Down Integer | Up Integer
        deriving (Show)
type Position = (Integer, Integer, Integer)

parseInput :: String -> [Command]
parseInput = map parseCommand . map words . lines

parseCommand :: [String] -> Command
parseCommand ["forward", x] = Forward (read x)
parseCommand ["down", x] = Down (read x)
parseCommand ["up", x] = Up (read x)

calculatePosition :: [Command] -> Position
calculatePosition = foldl positionUpdater (0, 0, 0)

positionUpdater :: Position -> Command -> Position
positionUpdater (x, depth, aim) (Forward z) = (x + z, depth + aim * z, aim)
positionUpdater (x, depth, aim) (Down z) = (x, depth, aim + z)
positionUpdater (x, depth, aim) (Up z) = (x, depth, aim - z)

positionValue :: Position -> Integer
positionValue (x, depth, _) = x * depth

main = do
        input <- getContents
        let commands = parseInput input
        putStrLn $ show $ positionValue $ calculatePosition commands
