import Text.Parsec
import Text.Parsec.String

data Snailfish = Pair Element Element
        deriving (Eq)
data Element = Elements Snailfish | Number Int
        deriving (Eq)

instance Show Snailfish where
        show (Pair e1 e2) = "[" ++ show e1 ++ "," ++ show e2 ++ "]"

instance Show Element where
        show (Elements s) = show s
        show (Number n) = show n

parseInput :: String -> [Snailfish]
parseInput = map parseSnailfish . lines

parseSnailfish :: String -> Snailfish
parseSnailfish str = case parse snailfish "" str of
                       Right s -> s
                       Left err -> error $ show err

snailfish :: Parser Snailfish
snailfish = do
        _ <- char '['
        e1 <- element
        _ <- char ','
        e2 <- element
        _ <- char ']'
        return $ Pair e1 e2

element :: Parser Element
element = elements <|> number

number :: Parser Element
number = do
        n <- many1 digit
        return $ Number (read n)

elements :: Parser Element
elements = do Elements <$> snailfish


main = do input <- getContents
          let snailfishes = parseInput input
          print snailfishes
