import Data.Array
import Text.Parsec
import Text.Parsec.String

gridMin = (-50, -50, -50)
gridMax = (50, 50, 50)

data StepType = On | Off
                deriving (Eq, Show)

type Coordinates = (Int, Int, Int)

data Step = Step StepType Coordinates Coordinates
            deriving (Show)

type Grid = Array Coordinates Bool

parseInput :: String -> [Step]
parseInput = map parseStep . lines

parseStep :: String -> Step
parseStep str = case parse step "" str of
                  Right s -> s
                  Left err -> error $ show err

step :: Parser Step
step = do
        stepType <- stepType
        _ <- space
        _ <- string "x="
        minX <- parseNumber
        _ <- string ".."
        maxX <- parseNumber
        _ <- char ','
        _ <- string "y="
        minY <- parseNumber
        _ <- string ".."
        maxY <- parseNumber
        _ <- char ','
        _ <- string "z="
        minZ <- parseNumber
        _ <- string ".."
        maxZ <- parseNumber
        return $ Step stepType (minX, minY, minZ) (maxX, maxY, maxZ)

stepType :: Parser StepType
stepType = try stepTypeOn <|> stepTypeOff
        where stepTypeOn = string "on" >> return On
              stepTypeOff = string "off" >> return Off

parseNumber :: Parser Int
parseNumber = do
        sign <- (char '-' >> return (-1)) <|> return 1
        integer <- unsignedInteger
        return $ sign * integer

unsignedInteger :: Parser Int
unsignedInteger = do
        digits <- many1 digit
        return $ read digits

initialGrid :: Grid
initialGrid = listArray (gridMin, gridMax) (repeat False)

runStep :: Grid -> Step -> Grid
runStep grid (Step onoff (minx, miny, minz) (maxx, maxy, maxz)) = grid // zip cubesWithinRange (repeat $ stepTypeToBool onoff)
        where cubes = [(x, y, z) | x <- [minx..maxx], y <- [miny..maxy], z <- [minz..maxz]]
              cubesWithinRange = filter (inRange (bounds grid)) cubes


runSteps :: Grid -> [Step] -> Grid
runSteps = foldl runStep

stepTypeToBool :: StepType -> Bool
stepTypeToBool On = True
stepTypeToBool Off = False

cubesOn :: Grid -> Int
cubesOn = length . filter id . elems

clipToBounds :: Step -> Step
clipToBounds (Step t (minx, miny, minz) (maxx, maxy, maxz)) = Step t (max minx gridminx, max miny gridminy, max minz gridminz)
                                                                     (min maxx gridmaxx, min maxy gridmaxy, min maxz gridmaxz)
        where (gridminx, gridminy, gridminz) = gridMin
              (gridmaxx, gridmaxy, gridmaxz) = gridMax

main = do input <- getContents
          let rebootSteps = parseInput input
              finalGrid = runSteps initialGrid (map clipToBounds rebootSteps)
          print $ cubesOn finalGrid
