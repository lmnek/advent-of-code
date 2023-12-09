import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (forM_)

inputFiles1 = [ "x_1" , "x_2" ]
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

-- DATA TYPES / CLASSES --------------


-- PARSING ----------------------------

parseLine = undefined

parseInput = undefined 

-- SOLUTION 1 -------------------------

--solve1 :: 
solve1 = undefined

-- SOLUTION 2 -------------------------

--solve2 :: 
solve2 = undefined

-- UTILS -------------------------


