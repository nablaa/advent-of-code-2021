import Data.List
import qualified Data.Map.Strict as M

type Rule = (String, Char)
type Rules = M.Map String Char
type Counts = M.Map Char Integer
type Solutions = M.Map (String, Int) Counts

parseInput :: String -> (String, [Rule])
parseInput input = (head ls, map rule ruleLines)
        where ls = lines input
              ruleLines = filter (/= "") $ tail ls
              rule line = (head (words line), head $ last (words line))

applyCountPairElements :: String -> Rules -> Int -> Solutions -> (Counts, Solutions)
applyCountPairElements [x, y] _ 0 solutions = (counts, M.insert ([x, y], 0) counts solutions)
        where counts = M.unionsWith (+) [M.fromList [(x, 1)], M.fromList [(y, 1)]]
applyCountPairElements pair@[x, y] rules n solutions = case M.lookup (pair, n) solutions of
                                                         Just c -> (c, solutions)
                                                         Nothing -> case M.lookup pair rules of
                                                                      Nothing -> undefined
                                                                      Just char -> (c, M.insert (pair, n) c solutions3)
                                                                        where pair1 = [x, char]
                                                                              pair2 = [char, y]
                                                                              (counts1, solutions2) = applyCountPairElements pair1 rules (n - 1) solutions
                                                                              (counts2, solutions3) = applyCountPairElements pair2 rules (n - 1) solutions2
                                                                              c = M.unionsWith (+) [counts1, counts2, M.fromList [(char, -1)]]
applyCountPairElements _ _ _ _ = undefined

stringPairs :: String -> [String]
stringPairs "" = []
stringPairs [_] = []
stringPairs (x:y:zs) = [x, y] : stringPairs (y:zs)

countPairsElements :: [String] -> Rules -> Int -> (Counts, Solutions)
countPairsElements pairs rules n = foldl f (M.empty, M.empty) pairs
        where f (counts, solutions) pair = let (c, s) = applyCountPairElements pair rules n solutions in (M.unionWith (+) counts c, s)

solve :: Int -> String -> [Rule] -> Integer
solve n str rules = last sorted - head sorted
        where (countMap, _) = countPairsElements (stringPairs str) (M.fromList rules) n
              removeMiddle = M.fromList $ map (\s -> (head s, fromIntegral (length s))) $ group $ sort $ tail $ reverse $ tail str
              finalCounts = M.unionWith (-) countMap removeMiddle
              counts = map snd $ M.toList finalCounts
              sorted = sort counts

solve1 :: String -> [Rule] -> Integer
solve1 = solve 10

solve2 :: String -> [Rule] -> Integer
solve2 = solve 40


main = do input <- getContents
          let (template, rules) = parseInput input
          print $ solve1 template rules
          print $ solve2 template rules
