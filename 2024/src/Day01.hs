module Day01 (main) where

import AOCUtils (runPrint, runReturn)
import Data.List (nub, sort, transpose)
import Text.Parsec (digit, endOfLine, many1, spaces)
import Text.Parsec.String (Parser)

inputFiles1 = ["1_1a", "1_1b"]

main :: IO ()
main = do
    runPrint inputFiles1 parseInput1 solve1 1
    runPrint inputFiles1 parseInput1 solve2 2

-- PARSING ----------------------------

number = read <$> many1 digit

parseLine = do
    num1 <- number
    spaces
    num2 <- number
    endOfLine
    return (num1, num2)

parseInput1 = do
    res <- many1 parseLine
    return (map fst res, map snd res)

-- SOLUTION 1 -------------------------

-- >>> runReturn inputFiles1 parseInput1 solve1 1
-- "1-1_1a: 11 ,  1-1_1b: 1320851"

solve1 :: ([Int], [Int]) -> Int
solve1 (l1, l2) = sum $ zipWith distance (sort l1) (sort l2)

distance x y = abs $ x - y

-- SOLUTION 2 -------------------------

-- >>> runReturn inputFiles1 parseInput1 solve2 2
-- "2-1_1a: 31 ,  2-1_1b: 26859182"

solve2 (left, right) = sum $ map similarity uniqueLeft
  where
    uniqueLeft = nub left
    similarity id = (count id left) * (count id right) * id

count x xs = length $ filter (x ==) xs
