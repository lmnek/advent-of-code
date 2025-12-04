{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use uncurry" #-}
module Day02 (main) where

import AOCUtils (runPrint, runReturn)
import Data.Array (Ix (range))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Tree (flatten)
import Text.Parsec (char, count, endBy, endOfLine, many1, sepBy, string, (<|>))
import Text.Parsec.Char (digit)
import Text.Parsec.String (Parser)

inputFiles1 = ["2_1a", "2_1b"]

main :: IO ()
main = do
    runPrint inputFiles1 parseInput1 solve1 1
    runPrint inputFiles1 parseInput1 solve2 2

-- CLASSES ----------------------------

type Range = (Int, Int)

-- SOLUTION 1 -------------------------

-- >>> runReturn inputFiles1 parseInput1 solve1 1
-- "1-2_1a: 1227775554 ,  1-2_1b: 43952536386"

solve1 :: [Range] -> Int
solve1 = sum . concatMap filterRange

filterRange (start, end) = filter (isInvalid . show) [start .. end]

isInvalid :: String -> Bool
isInvalid xs = fst halfs == snd halfs
  where
    halfs = splitAt (length xs `div` 2) xs

-- >>> runReturn inputFiles1 parseInput1 solve1b 1

-- note: more performant solution would be to just count the amount of
-- numbers that change in the first half of string
-- (for each one there will be one invalid second half, expect start/end)

-- solve1b :: [Range] -> [([Char], [Char], [Char], [Char])]
-- solve1b = map getInvalids2

-- solve1b = sum . concatMap getInvalids

-- getInvalids (start, end) = filter inRange $ map (\n -> let numStr = show n in read (numStr ++ numStr)) [fstHalf start .. fstHalf end]
getInvalids (start, end) = filter inRange $ map (\n -> let numStr = show n in read (numStr ++ numStr)) [fstHalf start .. fstHalf end]
  where
    fstHalf :: Int -> Int
    fstHalf xs = let strXs = show xs in read $ take (length strXs `div` 2) strXs
    inRange v = v >= start && v <= end

-- >>> getInvalids2 (898, 1012)
-- (8,98,10,12)
getInvalids2 (start, end) = (s1, s2, e1, e2)
  where
    halv xs = let (xs1, xs2) = splitAt (length xs `div` 2) xs in (read xs1, read xs2)
    (s1, s2) = halv $ show start
    (e1, e2) = halv $ show end
    midCount = e1 - s1 - 1
    startCount = if s2 >= s1 then 1 else 0
    endCount = if e2 <= e1 then 1 else 0

-- SOLUTION 2 -------------------------

-- >>> runReturn inputFiles1 parseInput1 solve2 2

solve2 _ = "todo"

-- PARSING ----------------------------

number = read <$> many1 digit
parseRange = do
    lowerBound <- number
    char '-'
    upperBound <- number
    return (lowerBound, upperBound)

parseInput1 :: Parser [Range]
parseInput1 = parseRange `sepBy` char ','
