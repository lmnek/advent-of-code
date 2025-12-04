{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module Day03 (main) where

import AOCUtils (runPrint, runReturn)
import Debug.Trace
import Text.Parsec (char, count, endBy, endOfLine, many1, sepBy, string, (<|>))
import Text.Parsec.Char (digit)
import Text.Parsec.String (Parser)

inputFiles1 = ["3_1a", "3_1b"]

main :: IO ()
main = do
    runPrint inputFiles1 parseInput1 solve1 1
    runPrint inputFiles1 parseInput1 solve2 2

-- SOLUTION 1 -------------------------
-- (23 min speedrun)

-- >>> runReturn inputFiles1 parseInput1 solve1 1
-- "1-3_1a: 357 ,  1-3_1b: 17144"

solve1 = sum . map solveRow

solveRow :: [Int] -> Int
solveRow bs = let (n1, n2) = iter 0 0 bs in n1 * 10 + n2

iter n1 n2 [x]
    | x > n2 = (n1, x)
    | otherwise = (n1, n2)
iter n1 n2 (x : xs)
    | x > n1 = iter x 0 xs
    | x > n2 = iter n1 x xs
    | otherwise = iter n1 n2 xs

-- SOLUTION 2 -------------------------

-- >>> runReturn inputFiles1 parseInput1 solve2 2
-- "2-3_1a: 3121910778619 ,  2-3_1b: 170371185255900"

solve2 :: [[Int]] -> Int
solve2 = sum . map solveRow2

solveRow2 xs = read $ concatMap show $ iter2 [] (replicate 12 0) 0 xs
  where
    xlen = length xs
    iter2 nsInit [] xi (x : xs) = nextCharIter nsInit xi xs
    iter2 nsInit nsRest _ [] = nsInit ++ nsRest
    iter2 nsInit ns'@(n : nsTail) xi xs'@(x : xs)
        | nsTailLen > xlen - xi - 1 = iter2 (nsInit ++ [n]) nsTail xi xs'
        | x > n = nextCharIter (nsInit ++ (x : replicate nsTailLen 0)) xi xs
        -- \| otherwise = nextCharIter (nsInit ++ ns') xi xs -- THIS FCKNIG HAUNTED ME TILL 1:17AM!!!
        | otherwise = iter2 (nsInit ++ [n]) nsTail xi xs'
      where
        nsTailLen = length nsTail
    nextCharIter ns xi xs = iter2 [] ns (xi + 1) xs

-- PARSING ----------------------------

parseDigit = do
    d <- digit
    return $ read [d]

parseInput1 = many1 parseDigit `endBy` endOfLine
