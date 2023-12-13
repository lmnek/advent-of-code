import AOCUtils (run)
import Text.Parsec
import Text.Parsec.String (Parser)

inputFiles1 = [ "4_1" , "4_2" ]
inputFiles2 = [ "4_1b" , "4_2" ]

main :: IO() 
main = do 
    run inputFiles1 parseInput solve1 1 
    run inputFiles2 parseInput solve2 2 

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

