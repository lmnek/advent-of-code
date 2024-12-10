module Day10 (main) where

import AOCUtils (runPrint, runReturn)
import Data.Array.Unboxed
import Data.Bifunctor
import Data.Function
import Data.List (group, singleton, sort)
import Data.Maybe (catMaybes)
import Text.Parsec
import Text.Parsec.String (Parser)

inputFiles1 = ["10_1b", "10_1c"]

main :: IO ()
main = do
    runPrint inputFiles1 inputP solve1 1

-- runPrint inputFiles1 inputP solve2 2

-- PARSING ----------------------------

type MapIndex = (Int, Int)
type Map = UArray MapIndex Int

posP = read . singleton <$> digit

inputP :: Parser Map
inputP = do
    heights2d <- many1 posP `endBy1` endOfLine
    let idxBounds = ((0, 0), (length heights2d - 1, subtract 1 $ length $ head heights2d))
    return $ listArray idxBounds $ concat heights2d

-- >>> runReturn inputFiles1 inputP solve1 1
-- "1-10_1b: 36 ,  1-10_1c: 459"
solve1 hss = sum $ map (uniqCount . trailCounts hss 0) $ startIdxs hss

startIdxs hss = [i | (i, h) <- assocs hss, h == 0]

-- trail ~ gradually going up from 0 to 9
-- trailCounts :: Map -> Int -> MapIndex -> Int
trailCounts hss height idx
    | height == 9 = [idx]
    | otherwise = concat [trailCounts hss (height + 1) nextIdx | nextIdx <- nextIdxs]
  where
    isAscending i = inRange (bounds hss) i && (hss ! i == height + 1)
    nextIdxs = filter isAscending $ map (addT idx) dirs

dirs = [(1, 0), (0, 1), (-1, 0), (0, -1)]
addT (y1, x1) (y2, x2) = (y1 + y2, x1 + x2)
uniqCount = length . group . sort

-- >>> runReturn inputFiles1 inputP solve2 2
-- "2-10_1b: 81 ,  2-10_1c: 1034"
solve2 hss = sum $ map (length . trailCounts hss 0) $ startIdxs hss
