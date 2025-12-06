{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Day06 (main) where

import AOCUtils
import Control.Monad (foldM)
import Data.List (transpose)
import Debug.Trace
import Text.Parsec
import Text.Parsec.Char (digit)
import Text.Parsec.String (Parser)

inputFiles1 = ["6_1a", "6_1b"]

main :: IO ()
main = do
    runPrint inputFiles1 parseInput1 solve1 1
    runPrint inputFiles1 parseInput2 solve2 2

-- SOLUTION 1 -------------------------
-- 30 mins (stuck on parsing) -> but so beautiful! thanks

-- >>> runReturn inputFiles1 parseInput1 solve1 1
-- "1-6_1a: 4277556 ,  1-6_1b: 5381996914800"

solve1 (rows, ops) = sum $ zipWith foldl1 ops $ transpose rows

-- PARSING 1 ----------------------------

spaces0 = many (char ' ')
spaces1 = many1 (char ' ')

parseNums = do
    spaces0
    nums <- number `sepBy` spaces1
    spaces0
    return nums

parseOps :: Parser (Int -> Int -> Int)
parseOps =
    (char '*' >> return (*))
        <|> (char '+' >> return (+))

parseInput1 = do
    rows <- parseNums `endBy` endOfLine
    spaces0
    ops <- parseOps `endBy` spaces1
    return (rows, ops)

-- SOLUTION 2 -------------------------
-- VERY VERY CLOSE to beatiful solution
-- -> but more more sophisticated parsing needed
-- (spaces before not enough - can be spaces after the previous number)

-- >>> runReturn [head inputFiles1] parseInput2 solve2 2
-- "2-6_1a: [[(0,\"123\"),(1,\"45\"),(2,\"6\")],[(0,\"328\"),(0,\"64\"),(0,\"98\")],[(1,\"51\"),(1,\"387\"),(1,\"215\")],[(0,\"64\"),(0,\"23\"),(0,\"314\")]]"

solve2 (rows, ops) = sum $ zipWith foldl1 ops $ map transform $ transpose rows

-- transform :: [(Int, String)] -> [Int]
transform ns =
    map
        (read . foldl foldlStep "")
        $ transpose
        $ map fillSpaces ns

fillSpaces (spsLen, numStr) = replicate spsLen Nothing ++ map Just numStr

foldlStep :: String -> Maybe Char -> String
foldlStep numStr m = case m of
    Just d -> numStr ++ [d]
    Nothing -> numStr

-- PARSING 2----------------------------

parseNum = do
    spsLen <- length <$> spaces0
    digits <- many1 digit
    return (spsLen, digits)

parseLine :: Parser [(Int, [Char])]
parseLine = do
    ns <- many1 parseNum
    spaces0
    -- note: need to remove the single space column between numbers,
    -- but its not there for the first col
    return (head ns : map (\(lens, ds) -> (lens - 1, ds)) (tail ns))

parseInput2 = do
    rows <- parseLine `endBy` endOfLine
    spaces0
    ops <- parseOps `endBy` spaces1
    return (rows, ops)
