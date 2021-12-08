import Data.Maybe


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

main = do input <- getContents
          let entries = parseInput input
          print $ calculate1478Counts entries
