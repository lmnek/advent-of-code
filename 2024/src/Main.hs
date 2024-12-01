module Main where

import Day01 (main)

-- IDK what to do with this file

main :: IO ()
main = do
    putStrLn "Enter the day number:"
    day <- getLine
    case day of
        "1" -> Day01.main
        _ -> putStrLn "Invalid day"
