module Day07 (main) where

import AOCUtils (runPrint, runReturn)
import Data.List
import Text.Parsec

inputFiles1 = ["7_1a", "7_1b"]

main :: IO ()
main = do
    runPrint inputFiles1 inputP solve1 1
    runPrint inputFiles1 inputP solve2 2

-- PARSING ----------------------------

numberP = read <$> many1 digit

equationP = do
    res <- numberP
    string ": "
    nums <- numberP `sepBy1` char ' '
    return (res, nums)

inputP = equationP `endBy1` endOfLine

-- >>> runReturn inputFiles1 inputP solve1 1
-- "1-7_1a: 3749 ,  1-7_1b: 1708857123053"

solve1 = solveWithOps [(+), (*)]

solveWithOps ops = sum . map fst . filter (isPossiblyTrue ops 0)

isPossiblyTrue ops acc (res, []) = res == acc
isPossiblyTrue ops acc (res, n : ns)
    | acc > res = False
    | otherwise = any (\op -> isPossiblyTrue ops (op acc n) (res, ns)) ops

-- >>> runReturn inputFiles1 inputP solve2 2

pipesOp a b = read (show a ++ show b)

solve2 = solveWithOps [(+), (*), pipesOp]
