import AOCUtils (run)
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.List.Extra (chunksOf)

inputFiles = [ "9_1" , "9_2" ]

main :: IO() 
main = do 
    run inputFiles parseInput solve1 1
    run inputFiles parseInput solve2 2

-- PARSING ----------------------------

integer :: Parser Int
integer = do
    sign <- optionMaybe (char '-')
    digits <- many1 digit
    return $ read $ (maybe "" (:[]) sign ++ digits)

parseLine = integer `sepBy` char ' ' <* endOfLine

parseInput = many1 parseLine

-- SOLUTION 1 -------------------------

solve1 :: [[Int]] -> Int
solve1 = sum . map extrapol

extrapol :: [Int] -> Int
extrapol row
    | all (==0) row = 0
    | otherwise = last row + (extrapol . nextRow $ row)

nextRow :: [Int] -> [Int]
nextRow row = map (\p -> snd p - fst p) $ pairs row

pairs :: [a] -> [(a, a)]
pairs xs = zip xs $ tail xs

-- SOLUTION 2 -------------------------

solve2 :: [[Int]] -> Int
solve2 = sum . map extrapol2

extrapol2 :: [Int] -> Int
extrapol2 row
    | all (==0) row = 0
    -- Only change is here:
    | otherwise = head row - (extrapol2 . nextRow $ row) 
