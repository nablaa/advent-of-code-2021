data Command = Forward Integer | Down Integer | Up Integer
        deriving (Show)
type Position = (Integer, Integer)

parseInput :: String -> [Command]
parseInput = map parseCommand . map words . lines

parseCommand :: [String] -> Command
parseCommand ["forward", x] = Forward (read x)
parseCommand ["down", x] = Down (read x)
parseCommand ["up", x] = Up (read x)

calculatePosition :: [Command] -> Position
calculatePosition = foldr positionUpdater (0, 0)

positionUpdater :: Command -> Position -> Position
positionUpdater (Forward z) (x, y) = (x + z, y)
positionUpdater (Down z) (x, y) = (x, y + z)
positionUpdater (Up z) (x, y) = (x, y - z)

positionValue :: Position -> Integer
positionValue (x, y) = x * y

main = do
        input <- getContents
        let commands = parseInput input
        putStrLn $ show $ positionValue $ calculatePosition commands
