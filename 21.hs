data Game = Game {
                players :: [Player],
                dice :: Dice
                 } deriving (Show)

data Player = Player {
                position :: Integer,
                score :: Integer
                     } deriving (Show)

data Dice = Dice {
                previousRoll :: Integer,
                rollCount :: Integer
                 } deriving (Show)

parseInput :: String -> [Integer]
parseInput = map ((read . last) . words) . lines

roll :: Dice -> (Integer, Dice)
roll (Dice previous count) = (next, Dice next (count + 1))
        where next = if previous == 100 then 1 else previous + 1

playStep :: Game -> Game
playStep (Game [] _ ) = error "need players"
playStep (Game (p:ps) d) = Game (ps ++ [p']) d'
        where (p', d') = advance p d

advance :: Player -> Dice -> (Player, Dice)
advance (Player pos s) d = (Player newPos newScore, d3)
        where (roll1, d1) = roll d
              (roll2, d2) = roll d1
              (roll3, d3) = roll d2
              newPos = advancePos pos [roll1, roll2, roll3]
              newScore = s + newPos

advancePos :: Integer -> [Integer] -> Integer
advancePos = foldl addPosition

addPosition :: Integer -> Integer -> Integer
addPosition pos 0 = pos
addPosition pos n | pos == 10 = addPosition 1 (n - 1)
                  | otherwise = addPosition (pos + 1) (n - 1)

playUntilWin :: Game -> Game
playUntilWin game | score (last (players game)) >= 1000 = game
                  | otherwise = playUntilWin $ playStep game

losingScoreAndRolled :: Game -> Integer
losingScoreAndRolled (Game ps (Dice _ rolls)) = rolls * score (head ps)


main = do input <- getContents
          let positions = parseInput input
              players = map (`Player` 0) positions
              d = Dice 100 0
              game = Game players d
          print game
          print $ losingScoreAndRolled $ playUntilWin game
