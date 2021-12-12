import Data.Char
import Data.List

parseInput :: String -> [(String, String)]
parseInput str = map parseLine (lines str)

parseLine :: String -> (String, String)
parseLine str = (n1, n2)
        where n1 = takeWhile (/= '-') str
              n2 = tail $ dropWhile (/= '-') str

small :: [(String, String)] -> [String]
small = nub . sort . filter (all isLower) . unzipEdges

big :: [(String, String)] -> [String]
big = nub . sort . filter (all isUpper) . unzipEdges

unzipEdges :: [(String, String)] -> [String]
unzipEdges entries = uncurry (++) $ unzip entries

printEdge :: (String, String) -> String
printEdge (x, y) = "edge(" ++ printLabel x ++ "," ++ printLabel y ++ ")."

printSmall :: String -> String
printSmall x = "small(" ++ printLabel x ++ ")."

printBig :: String -> String
printBig x = "big(" ++ printLabel x ++ ")."

printLabel :: String -> String
printLabel = map toLower

main = do input <- getContents
          let edges = parseInput input
          putStrLn $ unlines $ map printEdge edges
          putStrLn $ unlines $ map printSmall $ small edges
          putStrLn $ unlines $ map printBig $ big edges
