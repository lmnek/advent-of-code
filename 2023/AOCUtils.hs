module AOCUtils (run) where 

import Text.Parsec 
import Text.Parsec.String (Parser)
import Control.Monad (forM_)


run :: Show b => [String] -> Parser a -> (a -> b) -> Int -> IO() 
run inputFiles parser solve i = forM_ inputFiles $ doProblemsOnFile $ doProblem parser solve i 

doProblemsOnFile doFunc fileName = do
    input <- readFile $ "data/"++fileName
    doFunc input fileName

doProblem :: Show b => Parser a -> (a -> b) -> Int -> String -> String -> IO()
doProblem parser solve i input fileName = do
    let id = show i ++ "-" ++ fileName ++ ": "
    let res = case parse parser "" input  of
            Left err -> "parsing error: " ++ show err
            Right parsedData -> solution 
                where solution = show $ solve parsedData
    putStrLn $ id ++ res
