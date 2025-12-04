{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module Day03 (main) where

import AOCUtils (runPrint, runReturn)
import Debug.Trace
import Text.Parsec
import Text.Parsec.Char (digit)
import Text.Parsec.String (Parser)

inputFiles1 = ["4_1a", "4_1b"]

main :: IO ()
main = do
    runPrint inputFiles1 parseInput1 solve1 1
    runPrint inputFiles1 parseInput1 solve2 2

-- SOLUTION 1 -------------------------
-- 35 mins

-- >>> runReturn inputFiles1 parseInput1 solve1 1
-- "1-4_1a: 13 ,  1-4_1b: 1457"

solve1 pss = length $ concatMap (filter (< 4) . processLine) (enumerate pss)
  where
    xlen = length (head pss)
    ylen = length pss

    inBounds xi yi = xi >= 0 && yi >= 0 && xi < xlen && yi < ylen
    getPos xi yi = (pss !! yi) !! xi

    processLine (yi, ps) = map adjForPos $ filter ((== Roll) . snd) $ enumerate ps
      where
        adjForPos (xi, _) = sum $ map (checkPos xi yi) dirs

    checkPos xi yi (xd, yd) = checkPos' (xi + xd) (yi + yd)
    checkPos' xi yi
        | inBounds xi yi && getPos xi yi == Roll = 1
        | otherwise = 0

enumerate = zip [0 ..]

dirs = [(1, 0), (0, 1), (-1, 0), (0, -1), (1, 1), (-1, -1), (1, -1), (-1, 1)]

-- SOLUTION 2 -------------------------

-- >>> runReturn inputFiles1 parseInput1 solve2 2

-- perf: tail recursion acc
-- TODO: need to remove rolls after detected from pss !!!
-- (this is stupidly hard in FP compared to anything else??)
solve2 pss = if res == 0 then 0 else res + solve2 pss
  where
    res = solve1 pss

-- CLASSES ----------------------------

data Pos = Roll | EmptyPos deriving (Eq)

-- PARSING ----------------------------

parsePos =
    (char '.' >> return EmptyPos)
        <|> (char '@' >> return Roll)

parseInput1 = many1 parsePos `endBy` endOfLine
