import AOCUtils (run)
import Text.Parsec
import Text.Parsec.String (Parser)

inputFiles = [ "6_1", "6_2" ]

main :: IO() 
main = do
    run inputFiles parseInput1 solve1 1
    run inputFiles parseInput2 solve2 2

-- DATA TYPES / CLASSES --------------

data Range = R Int Int Int deriving Show
data SeedRange = SR Int Int

-- PARSING ----------------------------

number = read <$> many1 digit
spaces' = many1 $ char ' '

parseLine = do
    many1 letter
    char ':'
    spaces'
    nums <- number `sepBy` spaces'
    endOfLine
    return nums

parseInput1 = do
    times <- parseLine
    distances <- parseLine
    return $ zip times distances

parseLineNumber = read . concat . (map show) <$> parseLine

parseInput2 = do
    time <- parseLineNumber
    distance <- parseLineNumber
    return (time, distance)
    

-- SOLUTION 1 -------------------------

solve1 :: [(Int, Int)] -> Int
solve1 races = product $ map countBeaten races

countBeaten (time, dist) = time + 1 - notBeatenCount
    where notBeatenCount = 2 * until (\t -> getDist t time > dist) (+1) 0

getDist pressed whole = pressed * (whole - pressed)
    

-- SOLUTION 2 -------------------------

-- brute-force ~ 3 seconds 
-- NOTE: solvable with *Math*
-- PERF: binary search to find first win
solve2 :: (Int, Int) -> Int
solve2 race = solve1 [race]

