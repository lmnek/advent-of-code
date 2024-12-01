module AOCUtils_1 (runReturn) where

import Control.Monad (forM)
import Data.List (intercalate)
import Text.Parsec (parse)
import Text.Parsec.String (Parser)

runReturn :: (Show b) => [String] -> Parser a -> (a -> b) -> Int -> IO String
runReturn inputFiles parser solve i = do
    results <- forM inputFiles $ doProblemsOnFileReturn $ doProblemReturn parser solve i
    return $ intercalate " ,  " results

doProblemsOnFileReturn :: (String -> String -> String) -> String -> IO String
doProblemsOnFileReturn doFunc fileName = do
    input <- readFile $ "data/" ++ fileName
    return $ doFunc input fileName

doProblemReturn :: (Show b) => Parser a -> (a -> b) -> Int -> String -> String -> String
doProblemReturn parser solve i input fileName =
    let
        id = show i ++ "-" ++ fileName ++ ": "
        res = case parse parser "" input of
            Left err -> "parsing error: " ++ show err
            Right parsedData -> solution
              where
                solution = show $ solve parsedData
     in
        id ++ res
