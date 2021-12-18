type Coordinates = (Int, Int)
type Velocity = (Int, Int)
type Area = (Coordinates, Coordinates)
data Probe = Probe {
        position :: Coordinates,
        velocity :: Velocity
} deriving (Eq, Show)

parseInput :: String -> Area
parseInput str = ((read minx, read miny), (read maxx, read maxy))
        where w = words str
              xstr = init (drop 2 (w !! 2))
              minx = takeWhile (/= '.') xstr
              maxx = dropWhile (== '.') (drop (length minx) xstr)
              ystr = drop 2 (w !! 3)
              miny = takeWhile (/= '.') ystr
              maxy = dropWhile (== '.') (drop (length miny) ystr)

newProbe :: Int -> Int -> Probe
newProbe velx vely = Probe (0, 0) (velx, vely)

step :: Probe -> Probe
step (Probe (x, y) (velx, vely)) = Probe (nx, ny) (nvelx velx, nvely)
        where nx = x + velx
              ny = y + vely
              nvelx n | n == 0 = 0
                      | n < 0 = n + 1
                      | n > 0 = n - 1
                      | otherwise = undefined
              nvely = vely - 1

highestY :: [Probe] -> Int
highestY [] = 0
highestY ps = maximum $ map (snd . position) ps

generateSteps :: Probe -> [Probe]
generateSteps probe = stepped : generateSteps stepped
        where stepped = step probe

stepsUntilHitOrMiss :: Area -> Probe -> [Probe]
stepsUntilHitOrMiss area probe = takeWhileOneMore continueStepping (generateSteps probe)
        where continueStepping p@(Probe pos _) = not (withinArea area pos) && canStillHit area p

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []

hitsTarget :: Area -> Probe -> Bool
hitsTarget area probe | null steps = False
                      | otherwise = withinArea area (position lastStep)
        where steps = stepsUntilHitOrMiss area probe
              lastStep = last steps

visualize :: Area -> [Coordinates] -> String
visualize area@((aminx, aminy), (amaxx, amaxy)) probePositions = unlines [[drawPoint area probePositions x y | x <- [minx..maxx]] | y <- reverse [miny..maxy]]
        where minx = minimum $ 0 : aminx : map fst probePositions
              miny = minimum $ 0 : aminy : map snd probePositions
              maxx = maximum $ 0 : amaxx : map fst probePositions
              maxy = maximum $ 0 : amaxy : map snd probePositions

drawPoint :: Area -> [Coordinates] -> Int -> Int -> Char
drawPoint area probePositions 0 0 = 'S'
drawPoint area probePositions x y | (x, y) `elem` probePositions = '#'
                                  | withinArea area (x, y) = 'T'
                                  | otherwise = '.'

withinArea :: Area -> Coordinates -> Bool
withinArea ((aminx, aminy), (amaxx, amaxy)) (x, y) = x >= aminx && x <= amaxx && y >= aminy && y <= amaxy

canStillHit :: Area -> Probe -> Bool
canStillHit ((aminx, aminy), (amaxx, amaxy)) (Probe (x, y) (vx, vy)) | x > amaxx && vx >= 0 = False
                                                                     | y < aminy && vy < 0 = False
                                                                     | x < aminx && vx <= 0 = False
                                                                     | otherwise = True

velocityGenerator :: Area -> [Velocity]
velocityGenerator ((minx, miny), (maxx, _)) = concat [[(vx, vy) | vx <- [1..maxx]] | vy <- [miny..1000]]

probeHighestYs :: Area -> [Velocity] -> [Int]
probeHighestYs _ [] = []
probeHighestYs area ((vx, vy):vels) = if hitsTarget area probe
                                         then highestY (stepsUntilHitOrMiss area probe) : probeHighestYs area vels
                                         else probeHighestYs area vels
        where probe = newProbe vx vy

highestYHittingTarget :: Area -> [Velocity] -> Int
highestYHittingTarget area vels = maximum $ probeHighestYs area vels

main = do input <- getContents
          let area = parseInput input
              vels = velocityGenerator area
              ys = probeHighestYs area vels
          print ys
          print $ maximum ys
          print $ length ys
