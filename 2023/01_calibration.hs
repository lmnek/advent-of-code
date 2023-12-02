import Data.Char(isDigit)
import Data.List(isPrefixOf)

main :: IO() 
main = do
    fileName <- getLine
    contents <- readFile $ "data/"++fileName
    putStr "1: " 
    print . solve1 . parse $ contents
    putStr "2: "
    print . solve2 . parse $ contents

parse :: String -> [String]
parse = lines

solve1 :: [String] -> Int
solve1 = sum . map (value . filter isDigit)

value :: String -> Int 
value numbers = read [head numbers, head $ reverse numbers]

-- ------------------------

solve2 :: [String] -> Int
solve2 = solve1 . map translate 

nums :: [(Int, String)]
nums = zip [1..] ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

-- replace num strings with <num string digits num string>
translate :: String -> String
translate line = foldl translateWith line nums

translateWith :: String -> (Int, String) -> String
translateWith line (i, num) = replace line num (num++show i++num)

-- classic replace substring in string ...
replace :: String -> String -> String -> String
replace s@(x:xs) old new = if old `isPrefixOf` s
    then new ++ replace (drop (length old) s) old new
    else x : replace xs old new
replace [] _ _ = []
