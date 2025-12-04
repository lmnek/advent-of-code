module Day01 (main) where

import AOCUtils (runPrint, runReturn)
import Text.Parsec (char, endBy, endOfLine, many1, sepBy, (<|>))
import Text.Parsec.Char (digit)
import Text.Parsec.String (Parser)

inputFiles1 = ["1_1", "1_1"]

main :: IO ()
main = do
    runPrint inputFiles1 parseInput1 solve1 1
    runPrint inputFiles1 parseInput1 solve2 2

-- CLASSES ----------------------------

data Dir = L Int | R Int

-- SOLUTION 1 -------------------------

-- >>> runReturn inputFiles1 parseInput1 solve1 1
-- "1-1_1a: (32,3) ,  1-1_1b: (22,1023)"

solve1 = foldl dial (50, 0)

dial (cur, res) dir = (cur', res')
  where
    cur' = (cur + diff dir) `mod` 100
    res' = if cur' == 0 then res + 1 else res

diff (L num) = -num
diff (R num) = num

-- SOLUTION 2 -------------------------

-- >>> runReturn inputFiles1 parseInput1 solve2 2
-- "2-1_1a: (32,893) ,  2-1_1b: (22,3837515489832632992)"

-- >>> runReturn ["1_1a"] parseInput1 solve2 2
-- "2-1_1a: [(50,0),(82,-1),(52,-1),(0,1),(95,0),(55,1),(0,2),(99,1),(0,2),(14,2),(32,1)]"

solve2 = scanl dial2Clever (50, 0)

-- solve2 = foldl dial2 (50, 0)

-- dial2Stoopid (cur, res) dir = (cur'', res + resDiff1 + resDiff2)
--   where
--     cur' = (cur + diff dir)
--     resDiff2 = cur' `div` 100

-- broken and I have no time rip
dial2Clever (cur, res) dir = (cur'', res + resDiff1 + resDiff2)
  where
    cur' = cur + diff dir
    cur'' = cur' `mod` 100
    resDiff1 = if cur'' == 0 then 1 else 0
    resDiff2 = cur' `div` 100

-- PARSING ----------------------------

number = read <$> many1 digit

parseLine =
    L
        <$> (char 'L' *> number)
            <|> R
        <$> (char 'R' *> number)

parseInput1 = parseLine `endBy` endOfLine
