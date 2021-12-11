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

allSetsIntegers :: [String] -> [(Integer, [Char])]
allSetsIntegers patterns = [
          (0, collectSets 6 patterns),
          (1, collectSets 2 patterns),
          (2, collectSets 5 patterns),
          (3, collectSets 5 patterns),
          (4, collectSets 4 patterns),
          (5, collectSets 5 patterns),
          (6, collectSets 6 patterns),
          (7, collectSets 3 patterns),
          (8, collectSets 7 patterns),
          (9, collectSets 6 patterns)
          ]

printAllSets :: [(String, [Char])] -> String
printAllSets sets = unlines $ map formatSet sets
        where formatSet (name, elems) = name ++ "([" ++ intersperse ',' elems ++ "])."

isDigitValidForPattern :: Integer -> [Char] -> [(Integer, [Char])] -> Bool
isDigitValidForPattern n chars sets = all (`elem` set) chars
        where set = fromJust $ lookup n sets

permutationMatches :: [String] -> [(Integer, String)] -> Bool
permutationMatches _ [] = True
permutationMatches u ((n,chars):xs) = (isDigitValidForPattern n chars sets) && (permutationMatches u xs)
        where sets = allSetsIntegers u

allPermutations :: [String] -> [[(Integer, String)]]
allPermutations u = map (zip [0..]) (permutations u)

matchingPermutation :: [String] -> [(Integer, String)]
matchingPermutation u = head $ filter (permutationMatches u) (allPermutations u)




type WireMapping = [(Char, Char)]

wirePermutations :: [WireMapping]
wirePermutations = map (zip ['a'..'g']) (permutations ['a'..'g'])

isValidWireMapping :: [String] -> WireMapping -> Bool
isValidWireMapping patterns mapping  = all (correctDigitWithMapping mapping) patterns

correctDigitWithMapping :: WireMapping -> String -> Bool
correctDigitWithMapping mapping str = isDigitValid mappedDigit
        where mappedDigit = map mapWire str
              mapWire c = fromJust $ lookup c mapping

isDigitValid :: [Char] -> Bool
isDigitValid chars = any ((\ d -> sort d == sort chars) . snd) digitSegments
        where segments = map snd $ digitSegments

findMapping :: [String] -> WireMapping
findMapping patterns = head $ filter (isValidWireMapping patterns) wirePermutations

applyMapping :: WireMapping -> [String] -> [Integer]
applyMapping mapping = map (getDigit . map applyMappingStr)
        where applyMappingStr s = fromJust $ lookup s mapping

getDigit :: [Char] -> Integer
getDigit chars = fst $ fromJust $ find (matchChars chars) digitSegments

matchChars :: [Char] -> (Integer, [Char]) -> Bool
matchChars chars (_, d) = sort d == sort chars

outputValue :: [Integer] -> Integer
outputValue = foldl (\accum x -> (accum * 10) + x) 0

allOutputValues :: [([String], [String])] -> [Integer]
allOutputValues = map outputEntry

outputEntry :: ([String], [String]) -> Integer
outputEntry (patterns, output) = outputValue $ applyMapping mapping output
        where mapping = findMapping patterns


main = do input <- getContents
          let entries = parseInput input
          print $ calculate1478Counts entries
          print $ sum $ allOutputValues entries
