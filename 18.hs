import Text.Parsec
import Text.Parsec.String
import Data.Maybe

data Element = Pair Element Element | Number Int
        deriving (Eq)

instance Show Element where
        show (Pair e1 e2) = "[" ++ show e1 ++ "," ++ show e2 ++ "]"
        show (Number n) = show n

parseInput :: String -> [Element]
parseInput = map parseSnailfish . lines

parseSnailfish :: String -> Element
parseSnailfish str = case parse element "" str of
                       Right s -> s
                       Left err -> error $ show err

element :: Parser Element
element = elements <|> number

number :: Parser Element
number = do
        n <- many1 digit
        return $ Number (read n)

elements :: Parser Element
elements = do
        _ <- char '['
        e1 <- element
        _ <- char ','
        e2 <- element
        _ <- char ']'
        return $ Pair e1 e2

sumElements :: [Element] -> Element
sumElements = foldl1 addition

addition :: Element -> Element -> Element
addition e1 e2 = fromZipper reduced
        where p = Pair e1 e2
              zipper = toZipper p
              reduced = reduce zipper

reduce :: Zipper -> Zipper
reduce zipper = if t /= t2 then reduce splitted else splitted
        where exploded = runExplodes zipper
              splitted = splitLeftMostNumber (topMost exploded)
              t = topMost zipper
              t2 = topMost splitted

runExplodes :: Zipper -> Zipper
runExplodes zipper = if t /= t2 then runExplodes exploded else exploded
        where exploded = explodeLeftmostPair (topMost zipper)
              t = topMost zipper
              t2 = topMost exploded

data Trail = LeftPair Element | RightPair Element
        deriving (Show, Eq)
type Context = [Trail]
type Zipper = (Element, Context)

goLeft :: Zipper -> Maybe Zipper
goLeft (Number _, _) = Nothing
goLeft (Pair l r, trail) = Just (l, LeftPair r:trail)

goRight :: Zipper -> Maybe Zipper
goRight (Number _, _) = Nothing
goRight (Pair l r, trail) = Just (r, RightPair l:trail)

goUp :: Zipper -> Maybe Zipper
goUp (_, []) = Nothing
goUp (element, LeftPair r:trail) = Just (Pair element r, trail)
goUp (element, RightPair l:trail) = Just (Pair l element, trail)

topMost :: Zipper -> Zipper
topMost zipper = maybe zipper topMost (goUp zipper)

fromZipper :: Zipper -> Element
fromZipper = fst . topMost

toZipper :: Element -> Zipper
toZipper element = (element, [])

walk :: Zipper -> Context -> Zipper
walk zipper [] = zipper
walk zipper ((LeftPair _):cs) = walk (fromJust $ goLeft zipper) cs
walk zipper ((RightPair _):cs) = walk (fromJust $ goRight zipper) cs

explodeLeftmostPair :: Zipper -> Zipper
explodeLeftmostPair zipper@(Number _, trail) = zipper
explodeLeftmostPair z@(Pair (Number n1) (Number n2), trail) | length trail >= 4 = exploded
        where newZipper = (Number 0, trail)
              leftAdded = addLeftNumber n1 newZipper
              t = topMost leftAdded
              rightAdded = addRightNumber n2 (walk t (reverse trail))
              exploded = rightAdded
explodeLeftmostPair zipper@(_, trail) = exploded
        where leftExploded = maybe zipper explodeLeftmostPair (goLeft zipper)
              t = topMost leftExploded
              t2 = topMost zipper
              rightExploded = maybe leftExploded explodeLeftmostPair (goRight (walk t (reverse trail)))
              exploded = if t /= t2 then leftExploded else rightExploded

addLeftNumber :: Int -> Zipper -> Zipper
addLeftNumber = traverseLeftPoint

traverseLeftPoint :: Int -> Zipper -> Zipper
traverseLeftPoint n zipper@(_, []) = zipper
traverseLeftPoint n zipper@(_, (LeftPair _):trail) = traverseLeftPoint n (fromJust $ goUp zipper)
traverseLeftPoint n zipper@(_, (RightPair _):trail) = addRightmostNumber n (fromJust (goUp zipper >>= goLeft))

addRightmostNumber :: Int -> Zipper -> Zipper
addRightmostNumber n (Number x, trail) = (Number (x + n), trail)
addRightmostNumber n zipper = addRightmostNumber n (fromJust $ goRight zipper)

addRightNumber :: Int -> Zipper -> Zipper
addRightNumber = traverseRightPoint

traverseRightPoint :: Int -> Zipper -> Zipper
traverseRightPoint n zipper@(_, []) = zipper
traverseRightPoint n zipper@(_, (RightPair _):trail) = traverseRightPoint n (fromJust $ goUp zipper)
traverseRightPoint n zipper@(_, (LeftPair _):trail) = addLeftmostNumber n (fromJust (goUp zipper >>= goRight))

addLeftmostNumber :: Int -> Zipper -> Zipper
addLeftmostNumber n (Number x, trail) = (Number (x + n), trail)
addLeftmostNumber n zipper = addLeftmostNumber n (fromJust $ goLeft zipper)

splitLeftMostNumber :: Zipper -> Zipper
splitLeftMostNumber zipper@(Number n, trail) | n >= 10 = (splitNumber n, trail)
                                             | otherwise = zipper
splitLeftMostNumber zipper@(Pair _ _, trail) = splitted
        where splittedLeft = splitLeftMostNumber (fromJust $ goLeft zipper)
              t = topMost splittedLeft
              t2 = topMost zipper
              splittedRight = splitLeftMostNumber (fromJust $ goRight zipper)
              splitted = if t /= t2 then splittedLeft else splittedRight

splitNumber :: Int -> Element
splitNumber n = Pair (Number n1) (Number n2)
        where n1 = n `div` 2
              n2 = ceiling (fromIntegral n / 2)


main = do input <- getContents
          let snailfishes = parseInput input
          print $ sumElements snailfishes
