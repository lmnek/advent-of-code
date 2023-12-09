import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (forM_)
import Data.List.Extra (chunksOf)

inputFiles1 = [ "9_1" , "9_2" ]
inputFiles2 = inputFiles1

main :: IO() 
main = do 
    forM_ inputFiles1 $ doProblemsOnFile $ doProblem parseInput solve1 1 
    forM_ inputFiles2 $ doProblemsOnFile $ doProblem parseInput solve2 2

doProblemsOnFile doFunc fileName = do
    input <- readFile $ "data/"++fileName
    doFunc input

doProblem :: Show b => Parser a -> (a -> b) -> Int -> String -> IO()
doProblem parseData solve idx input = do
    let str = case parse parseData "" input  of
            Left err -> "Parsing error: " ++ show err
            Right parsedData -> show idx ++ ": " ++ solution 
                where solution = show $ solve parsedData
    putStrLn str

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
