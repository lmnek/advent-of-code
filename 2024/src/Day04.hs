module Day04 (main) where

import AOCUtils (runPrint, runReturn)
import Data.Bifunctor (bimap)
import Data.List
import Text.Parsec
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (manyTill)

inputFiles1 = ["4_1a", "4_1b"]

main :: IO ()
main = do
    runPrint inputFiles1 parseInput1 solve1 1
    runPrint inputFiles1 parseInput1 solve2 2

-- PARSING ----------------------------

parseInput1 = many $ manyTill anyChar endOfLine

-- >>> runReturn inputFiles1 parseInput1 solve1 1
-- "1-4_1a: 18 ,  1-4_1b: 2575"

posDirs = [(1, 0), (0, 1), (1, 1), (1, -1)]
dirs = posDirs ++ map (bimap negate negate) posDirs

-- NOTE: !! for list has O(n) access time -> uarray would significantly increase performance

solve1 :: [[Char]] -> Int
solve1 xss@(row : _) =
    sum $
        [1 | y <- [0 .. length xss - 1], x <- [0 .. length row - 1], dir <- dirs, includesWord xss y x "XMAS" dir]

includesWord xss@(row : _) y x word (y_dir, x_dir) = iter y x word
  where
    iter _ _ [] = True
    iter y x (c : w)
        | y < 0 || x < 0 || x >= length row || y >= length xss = False
        | ((xss !! y) !! x) == c = iter (y + y_dir) (x + x_dir) w
        | otherwise = False

-- >>> runReturn inputFiles1 parseInput1 solve2 2
-- "2-4_1a: 9 ,  2-4_1b: 2041"

dirs2 = [(1, 1), (1, -1), (-1, -1), (-1, 1)]

solve2 xss@(row : xs) =
    length . filter (== 2) . map length . group . sort $
        [ (y + y_dir, x + x_dir)
        | y <- [0 .. length xss - 1]
        , x <- [0 .. length row - 1]
        , dir@(y_dir, x_dir) <- dirs2
        , includesWord xss y x "MAS" dir
        ]
