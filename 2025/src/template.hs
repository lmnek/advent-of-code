module Template (main) where

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

-- >>> runReturn inputFiles1 parseInput1 solve1 1

solve1 _ = "todo"

-- SOLUTION 2 -------------------------

-- >>> runReturn inputFiles1 parseInput1 solve2 2

solve2 _ = "todo"

-- CLASSES ----------------------------

parseLine = string "todo"

parseInput1 = parseLine `endBy` endOfLine
