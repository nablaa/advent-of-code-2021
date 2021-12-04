import Data.List

data Game = Game {
            numbers :: [Integer]
          , lastDrawn :: Integer
          , boards :: [Board]
          } deriving (Show)

data Board = Board {
           boardCells :: [[Cell]]
           } deriving (Show)

data Cell = Cell {
            value :: Integer
          , marked :: Bool
          } deriving (Show)

parseInput :: String -> Game
parseInput input = Game (parseNumbers numbersLine) 0 (parseBoards boardLines)
        where allLines = lines input
              numbersLine = head allLines
              boardLines = drop 2 allLines

parseNumbers :: String -> [Integer]
parseNumbers = map read . wordsWhen (== ',')

parseBoards :: [String] -> [Board]
parseBoards strs = reverse (parseBoards' strs [])

parseBoards' :: [String] -> [Board] -> [Board]
parseBoards' [] boards = boards
parseBoards' lines boards = parseBoards' nextBoardLines (parseBoard boardLines : boards)
        where boardLines = takeWhile isNotEmptyLine lines
              rest = dropWhile isNotEmptyLine lines
              nextBoardLines = if rest == [] then rest else tail rest
              isNotEmptyLine = \x -> x /= ""

parseBoard :: [String] -> Board
parseBoard strs = Board (map (map toCell . words) strs)
        where toCell str = Cell (read str) False

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                        "" -> []
                        s' -> w : wordsWhen p s''
                                where (w, s'') = break p s'

markMatchingCell :: Integer -> Board -> Board
markMatchingCell number (Board cells) = Board newCells
        where newCells = map markRow cells
              markRow = map (markCell number)

markCell :: Integer -> Cell -> Cell
markCell number (Cell value marked) | value == number = Cell value True
                                    | otherwise = Cell value marked

unmarkedCells :: Board -> [Cell]
unmarkedCells (Board cells) = filter (\cell -> marked cell == False) (concat cells)

isWinning :: Board -> Bool
isWinning = any isWinningLine . possibleLines
        where isWinningLine = all marked

possibleLines :: Board -> [[Cell]]
possibleLines (Board cells) = cells ++ transpose cells

playRound :: Game -> Game
playRound (Game (x:xs) _ boards) = Game xs x newBoards
        where newBoards = map (markMatchingCell x) boards

winningBoards :: Game -> [Board]
winningBoards (Game _ _ boards) = filter isWinning boards

playUntilWinner :: Game -> Game
playUntilWinner game | null (winningBoards game) = playUntilWinner (playRound game)
                     | otherwise = game

score :: Game -> Integer
score game@(Game _ lastDrawn boards) = sumUnmarked * lastDrawn
        where board = head $ winningBoards game
              sumUnmarked = sum $ map value $ unmarkedCells board


main = do input <- getContents
          putStrLn input
          let game = parseInput input
          putStrLn $ show game
