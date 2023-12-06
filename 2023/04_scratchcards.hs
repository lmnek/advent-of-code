import Text.Parsec
import Text.Parsec.String (Parser)
import Debug.Trace 

main :: IO() 
main = do
    fileName <- getLine
    input <- readFile $ "data/"++fileName
    doProblem input parseInput solve1 1
    doProblem input parseInput solve2 2

doProblem :: Show b => String -> Parser a -> (a -> b) -> Int -> IO()
doProblem input parseData solve idx = do
    let str = case parse parseData "" input  of
            Left err -> "Parsing error: " ++ show err
            Right parsedData -> show idx ++ ": " ++ solution 
                where solution = show $ solve parsedData
    putStrLn str

-- DATA TYPES / CLASSES --------------

data Card = Card Int [Int] [Int] deriving Show 

-- PARSING ----------------------------

number :: Parser Int
number = read <$> many1 digit
spaces' = many $ char ' '

parseNums = spaces >> number `endBy` spaces'

parseLine = do
    string "Card"
    spaces'
    cardNum <- number
    char ':'
    winning <- parseNums
    char '|'
    have <- parseNums
    endOfLine
    return $ Card (cardNum - 1) winning have

parseInput = many parseLine

-- SOLUTION 1 -------------------------

solve1 :: [Card] -> Int 
solve1 = sum . (map cardPoints)

cardPoints card = case winCount card of
        0  -> 0
        wc -> 2 ^ (wc - 1) 

winCount (Card _ ws hs) = count' isWin hs
    where isWin h = h `elem` ws

-- SOLUTION 2 -------------------------

solve2 :: [Card] -> Int
solve2 cards = sum $ foldl process [1 | _ <- cards] cards

process :: [Int] -> Card -> [Int]
process counts card@(Card cardIdx ws hs) = mapWithIndex updateCounts counts
    where points = winCount card
          cardCount = counts!!cardIdx
          updateCounts idx c
            | idx > cardIdx && idx <= (cardIdx + points) = c + cardCount
            | otherwise = c


-- UTILS -------------------------

count' :: (a -> Bool) -> [a] -> Int
count' pred xs = length $ filter pred xs

mapWithIndex f xs = zipWith f [0..] xs

