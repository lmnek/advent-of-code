import Text.Parsec
import Text.Parsec.String (Parser)
import Data.List (sort, sortBy, group, maximumBy)
import Data.Map (Map, fromListWith, elems, keys, toList)
import Data.List.Extra (sortOn)
import Data.Function (on)

main :: IO() 
main = do
    fileName <- getLine
    input <- readFile $ "data/"++fileName
    doProblem input parseInput1 solve1 1
    doProblem input parseInput2 solve1 2

doProblem :: Show b => String -> Parser a -> (a -> b) -> Int -> IO()
doProblem input parseData solve idx = do
    let str = case parse parseData "" input  of
            Left err -> "Parsing error: " ++ show err
            Right parsedData -> show idx ++ ": " ++ solution 
                where solution = show $ solve parsedData
    putStrLn str

-- PARSING ----------------------------

toCardValue :: Int -> Char -> Int
toCardValue jVal card = 
    case lookup card cardValues of
        Just value -> value
        Nothing    -> read [card]
    where cardValues = [('A', 14), ('K', 13), ('Q', 12), ('J', jVal), ('T', 10)]

parseCard :: Int -> Parser Int
parseCard jVal = toCardValue jVal <$> noneOf " "

parseLine :: Int -> Parser ([Int], Int)
parseLine jVal = do
    cards <- many1 $ parseCard jVal
    char ' '
    bet <- read <$> many1 digit
    many $ char ' '
    endOfLine
    return $ (cards, bet)

parseInput1 = many1 $ parseLine 11
parseInput2 = many1 $ parseLine 0

-- SOLUTION 1 -------------------------

solve1 = foldl updateRes 0 . zip [1..] . map snd . sortRounds 
    where updateRes sum (i, bid) = sum + i * bid

sortRounds = sortBy (compHand `on` fst) 

compHand = compHandType <> compOrder

compHandType :: [Int] -> [Int] -> Ordering
compHandType = compare `on` sortedFreqWithJoker

sortedFreq :: [Int] -> [Int]
sortedFreq = reverse . sort . elems . freq

compOrder = compare

-- SOLUTION 2 -------------------------

-- replace "sortedFreq" with this in compHandType function
sortedFreqWithJoker :: [Int] -> [Int]
sortedFreqWithJoker xs = sortedFreq $ map replaceJokers xs 
    where xsWithoutJ = filter (/= 0) xs
          mostCom = if length xsWithoutJ == 0 then 0 else mostCommon xsWithoutJ
          replaceJokers x
            | x == 0 = mostCom
            | otherwise = x

mostCommon = head . maximumBy (compare `on` length) . group . sort 

-- UTILS ------------------------------

freq :: [Int] -> Map Int Int
freq xs = fromListWith (+) [(c, 1) | c <- xs]

count' x = length . filter (x==)
