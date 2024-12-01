module Main where

import Day01 (solve1)

-- IDK what to do with this file

main :: IO ()
main = do
    putStrLn "Enter the day number:"
    day <- getLine
    case day of
        "1" -> putStrLn "1"
        -- "2" -> Day02.solve
        -- Add more days as needed
        _ -> putStrLn "Invalid day"
