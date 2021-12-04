import Data.List

data Game = Game {
            numbers :: [Integer]
          , lastDrawn :: Integer
          , boards :: [Board]
          , winningBoards :: [Board]
          } deriving (Show)

newtype Board = Board {
           boardCells :: [[Cell]]
           } deriving (Show)

data Cell = Cell {
            value :: Integer
          , marked :: Bool
          } deriving (Show)

parseInput :: String -> Game
parseInput input = Game (parseNumbers numbersLine) 0 (parseBoards boardLines) []
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
              nextBoardLines = if null rest then rest else tail rest
              isNotEmptyLine = (/= "")

parseBoard :: [String] -> Board
parseBoard strs = Board (map (map toCell . words) strs)
        where toCell str = Cell (read str) False

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                        "" -> []
                        s' -> w : wordsWhen p s''
                                where (w, s'') = break p s'

markMatchingCell :: Integer -> Board -> Board
markMatchingCell _ board | isWinning board = board
markMatchingCell number (Board cells) = Board newCells
        where newCells = map markRow cells
              markRow = map (markCell number)

markCell :: Integer -> Cell -> Cell
markCell number (Cell value marked) | value == number = Cell value True
                                    | otherwise = Cell value marked

unmarkedCells :: Board -> [Cell]
unmarkedCells (Board cells) = filter (not . marked) (concat cells)

isWinning :: Board -> Bool
isWinning = any isWinningLine . possibleLines
        where isWinningLine = all marked

possibleLines :: Board -> [[Cell]]
possibleLines (Board cells) = cells ++ transpose cells

playRound :: Game -> Game
playRound (Game (x:xs) _ boards boardsWon) = Game xs x newInProgressBoards (boardsWon ++ newWinningBoards)
        where newBoards = map (markMatchingCell x) boards
              newWinningBoards = filter isWinning newBoards
              newInProgressBoards = filter (not . isWinning) newBoards
playRound game = undefined

playUntilWinner :: Game -> Game
playUntilWinner game | null (winningBoards game) = playUntilWinner (playRound game)
                     | otherwise = game

playTilEnd :: Game -> Game
playTilEnd game@(Game _ _ [] _) = game
playTilEnd game@(Game [] _ _ _) = game
playTilEnd game = playTilEnd (playRound game)

score :: Game -> Integer
score game@(Game _ lastDrawn _ boards) = sumUnmarked * lastDrawn
        where board = head boards
              sumUnmarked = sum $ map value $ unmarkedCells board

scoreLastWinning :: Game -> Integer
scoreLastWinning game@(Game _ lastDrawn _ boards) = sumUnmarked * lastDrawn
        where board = last boards
              sumUnmarked = sum $ map value $ unmarkedCells board

main = do input <- getContents
          let game = parseInput input
              finishedGame = playUntilWinner game
              endGame = playTilEnd game
          print $ score finishedGame
          print $ scoreLastWinning endGame
