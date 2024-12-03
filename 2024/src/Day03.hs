module Day03 (main) where

import AOCUtils (runPrint, runReturn)
import Control.Monad
import Data.List
import Data.Maybe
import Data.Type.Equality
import Text.Parsec
import Text.Parsec.Char (char)

inputFiles1 = ["3_1a", "3_1b"]
inputFiles2 = ["3_2a", "3_1b"]

main :: IO ()
main = do
    runPrint inputFiles1 parseInput1 solve1 1
    runPrint inputFiles2 parseInput2 solve2 2

-- CLASSES ----------------------------

data Instr = Mul Int Int | Do | Dont

-- PARSING ----------------------------

numberP = read <$> many1 digit

mulP = do
    string "mul("
    x <- numberP
    char ','
    y <- numberP
    char ')'
    return (x, y)

mulOrCharP = try (Just <$> mulP) <|> (anyChar >> return Nothing)
parseInput1 = catMaybes <$> many mulOrCharP

----

mulInstrP = uncurry Mul <$> mulP
doInstrP = string "do()" >> return Do
dontInstrP = string "don't()" >> return Dont

tryInstrP instrP = try (Just <$> instrP)

instrP =
    tryInstrP mulInstrP
        <|> tryInstrP doInstrP
        <|> tryInstrP dontInstrP
        <|> (anyChar >> return Nothing)

parseInput2 = catMaybes <$> many instrP

-- >>> runReturn inputFiles1 parseInput1 solve1 1
-- "1-3_1a: 161 ,  1-3_1b: 161085926"

solve1 :: [(Int, Int)] -> Int
solve1 = sum . map (uncurry (*))

-- >>> runReturn inputFiles2 parseInput2 solve2 2
-- "2-3_2a: 48 ,  2-3_1b: 82045421"

solve2 :: [Instr] -> Int
solve2 = snd . foldl eval (True, 0)

eval (_, sum) Do = (True, sum)
eval (_, sum) Dont = (False, sum)
eval (False, sum) (Mul _ _) = (False, sum)
eval (True, sum) (Mul x y) = (True, sum + (x * y))
