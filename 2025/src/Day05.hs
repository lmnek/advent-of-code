-- https://adventofcode.com/2025/day/5
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module Day05 (main) where

import AOCUtils (runPrint, runReturn)
import Data.Foldable (find)
import Debug.Trace
import Text.Parsec
import Text.Parsec.Char (digit)
import Text.Parsec.String (Parser)

inputFiles1 = ["5_1a", "5_1b"]

main :: IO ()
main = do
    runPrint inputFiles1 parseInput1 solve1 1
    runPrint inputFiles1 parseInput1 solve2 2

-- SOLUTION 1 -------------------------

-- >>> runReturn inputFiles1 parseInput1 solve1 1
-- "1-5_1a: 3 ,  1-5_1b: 607"

-- Start off with the most naive solution (not even overlap merging) -> good enough here!
solve1 (rs, ids) = length $ filter id $ map (inRanges rs) ids

inRanges rs id = case find (inRange id) rs of
    Just _ -> True
    Nothing -> False

inRange id (s, e) = id >= s && id <= e

-- SOLUTION 2 -------------------------

-- >>> runReturn inputFiles1 parseInput1 solve2 2
-- "2-5_1a: 14 ,  2-5_1b: 342433357244012"

-- probably could be done better (avoid repeated merging via untilNoShrink):
-- - when an merged interval -> immidiately run it through the list of resulting ranges again
-- - interval tree? and related algo

-- solve2 (rs, _) = untilNoShrink mergeStep rs
solve2 (rs, _) = sum $ map (\(s, e) -> e - s + 1) $ untilNoShrink mergeStep rs
  where
    mergeStep rs' = foldl mergeMulti [] rs'

-- note: could be tail recursed
mergeMulti [] r' = [r']
mergeMulti (r : rsRest) r' =
    -- case merge r' r <|> merge r r' of
    case merge r' r of
        Just merged -> merged : rsRest
        Nothing -> r : mergeMulti rsRest r'

merge (s, e) (s', e')
    | e < s' || e' < s = Nothing
    | otherwise = Just (min s s', max e e')

untilNoShrink f xs =
    let ys = f xs
     in if length ys < length xs
            then untilNoShrink f ys
            else xs

-- Initial one side version:
-- merge (s, e) (s', e')
--     | s <= s' && e >= e' = Just (s, e)
--     | s < s' && e > s' = Just (s, max e e')
--     | e > e' && s < e' = Just (min s s', e)
--     -- \| abs (s' - e) == 1 = Just (s, e')
--     | otherwise = Nothing

-- CLASSES ----------------------------

number :: Parser Int
number = read <$> many1 digit

parseRangeLine = do
    s <- number
    char '-'
    e <- number
    endOfLine
    return (s, e)

parseInput1 = do
    ranges <- many1 parseRangeLine
    endOfLine
    ids <- number `endBy` endOfLine
    return (ranges, ids)
