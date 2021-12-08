import Data.Maybe
import Data.Ord
import Data.Function
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M


digitSegments :: [(Integer, [Char])]
digitSegments = [
        (0, ['a', 'b', 'c', 'e', 'f', 'g']),
        (1, ['c', 'f']),
        (2, ['a', 'c', 'd', 'e', 'g']),
        (3, ['a', 'c', 'd', 'f', 'g']),
        (4, ['b', 'c', 'd', 'f']),
        (5, ['a', 'b', 'd', 'f', 'g']),
        (6, ['a', 'b', 'd', 'e', 'f', 'g']),
        (7, ['a', 'c', 'f']),
        (8, ['a', 'b', 'c', 'd', 'e', 'f', 'g']),
        (9, ['a', 'b', 'c', 'd', 'f', 'g'])
        ]

parseInput :: String -> [([String], [String])]
parseInput = map parseLine . lines

parseLine :: String -> ([String], [String])
parseLine str = (words uniquePatternStr, words digitOutputValue)
        where uniquePatternStr = takeWhile (/= '|') str
              digitOutputValue = drop 1 $ dropWhile (/= '|') str

calculate1478Counts :: [([String], [String])] -> Int
calculate1478Counts entries = length outputWithLengths
        where outputValues = concatMap snd entries
              lengths = map (length . fromJust . (`lookup` digitSegments)) [1, 4, 7, 8]
              outputWithLengths = filter (\x -> any (\y -> length x == y) lengths) outputValues

collectSets :: Int -> [String] -> [Char]
collectSets n = nub . sort . concat . filter (\x -> length x == n)

allSets :: [String] -> [(String, [Char])]
allSets patterns = [
          ("zeros", collectSets 6 patterns),
          ("ones", collectSets 2 patterns),
          ("twos", collectSets 5 patterns),
          ("threes", collectSets 5 patterns),
          ("fours", collectSets 4 patterns),
          ("fives", collectSets 5 patterns),
          ("sixes", collectSets 6 patterns),
          ("sevens", collectSets 3 patterns),
          ("eights", collectSets 7 patterns),
          ("nines", collectSets 6 patterns)
          ]

printAllSets :: [(String, [Char])] -> String
printAllSets sets = unlines $ map formatSet sets
        where formatSet (name, elems) = name ++ "([" ++ intersperse ',' elems ++ "])."

main = do input <- getContents
          let entries = parseInput input
          print $ calculate1478Counts entries
