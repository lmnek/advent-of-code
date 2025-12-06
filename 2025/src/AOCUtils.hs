module AOCUtils (runPrint, runReturn, number) where

import Control.Monad (forM, forM_)
import Data.List (intercalate)
import Text.Parsec
import Text.Parsec.String (Parser)

-- TODO: fix not printing
runPrint :: (Show b) => [String] -> Parser a -> (a -> b) -> Int -> IO ()
runPrint inputFiles parser solve i = forM_ inputFiles $ doProblemsOnFile putStrLn $ doProblem parser solve i

runReturn :: (Show b) => [String] -> Parser a -> (a -> b) -> Int -> IO String
runReturn inputFiles parser solve i = do
    results <- forM inputFiles $ doProblemsOnFile id $ doProblem parser solve i
    return $ intercalate " ,  " results

doProblemsOnFile :: (String -> a) -> (String -> String -> String) -> String -> IO a
doProblemsOnFile transform doFunc fileName = do
    input <- readFile $ "data/" ++ fileName
    return $ transform $ doFunc input fileName

doProblem :: (Show b) => Parser a -> (a -> b) -> Int -> String -> String -> String
doProblem parser solve i input fileName =
    let
        id = show i ++ "-" ++ fileName ++ ": "
        res = case parse parser "" input of
            Left err -> "parsing error: " ++ show err
            Right parsedData -> solution
              where
                solution = show $ solve parsedData
     in
        id ++ res

-- -- UTILS -----

number :: Parser Int
number = read <$> many1 digit
