module Day02 (main) where

import AOCUtils (runPrint, runReturn)
import Data.List (elemIndices, findIndices, inits, tail, tails)
import Text.Parsec (digit, endBy1, endOfLine, many1, sepBy1)
import Text.Parsec.Char (char)
import Text.Parsec.String (Parser)

inputFiles1 = ["2_1a", "2_1b"]

main :: IO ()
main = do
    runPrint inputFiles1 parseInput1 solve1 1
    runPrint inputFiles1 parseInput1 solve2 2

-- PARSING ----------------------------

number = read <$> many1 digit
line = number `sepBy1` char ' '
parseInput1 = line `endBy1` endOfLine

-- >>> runReturn inputFiles1 parseInput1 solve1 1
-- "1-2_1a: 2 ,  1-2_1b: 524"

solve1 :: [[Int]] -> Int
solve1 = length . filter id . map isSafe

isSafe :: [Int] -> Bool
isSafe l@(x : y : _) = isSafeWithOrder l
  where
    flip res = if x > y then res else -res
    isPairSafe a b = flip (a - b) `elem` [1 .. 3]

    isSafeWithOrder [x] = True
    isSafeWithOrder (x : y : xs)
        | isPairSafe x y = isSafeWithOrder (y : xs)
        | otherwise = False

-- Alternative approach to first part -> less efficient but cleaner
-- >>> runReturn inputFiles1 parseInput1 solve1B 1
-- "1-2_1a: 2 ,  1-2_1b: 524"

solve1B :: [[Int]] -> Int
solve1B = length . filter id . map isSafeB

isSafeB xs@(x : y : _) = and $ zipWith checkPair xs (tail xs)
  where
    flip a = if x > y then a else -a
    checkPair a b = flip (a - b) `elem` [1 .. 3]

-- More efficitent solution but there is a bug somewhere ://

-- >>> runReturn inputFiles1 parseInput1 solve2 2
-- "2-2_1a: 2 ,  2-2_1b: 549"

solve2 :: [[Int]] -> Int
solve2 = length . filter id . map isSafe2

isSafe2 xs@(x : y : _) = eval $ elemIndices False $ zipWith checkPair xs (tail xs)
  where
    flip a = if x > y then a else -a
    checkPair a b = flip (a - b) `elem` [1 .. 3]

    -- Check for single toleration
    eval [] = True
    -- unsafe at start/end of line -> ok (also need to check when in middle and remove)
    eval [i] = i == 0 || i == length xs - 2
    eval [i, j]
        -- two unsafe pairs next to each other
        | (abs i - j) == 1 = checkPair (xs !! i) (xs !! (j + 1)) -- check when removed
        | otherwise = False
    eval _ = False

-- Brute-force solution

-- >>> runReturn inputFiles1 parseInput1 solve2b 2
-- "2-2_1a: 4 ,  2-2_1b: 569"

solve2b :: [[Int]] -> Int
solve2b = length . filter id . map (any isSafeB . variations)
  where
    variations ls = ls : zipWith (++) (inits ls) (tail (tails ls))
