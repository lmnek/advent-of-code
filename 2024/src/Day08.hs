module Day08 (main) where

import AOCUtils (runPrint, runReturn)
import Data.Function
import Data.List
import Text.Parsec

inputFiles1 = ["8_1a", "8_1b"]

main :: IO ()
main = do
    runPrint inputFiles1 inputP solve1 1

-- runPrint inputFiles1 inputP solve2 2

-- PARSING ----------------------------

getPos = do
    posI <- getPosition
    return (sourceLine posI - 1, sourceColumn posI - 1)

getColCountP = do
    many $ noneOf "\n"
    posI <- getPosition
    endOfLine
    return $ sourceColumn posI - 1

emptyP = many $ oneOf ".\n"

antennaP = do
    emptyP
    pos <- getPos
    c <- noneOf ".\n"
    emptyP
    return (c, pos)

inputP = do
    colCount <- lookAhead getColCountP
    antennas <- many1 antennaP
    rowCount <- fst <$> getPos
    return (rowCount, colCount, antennas)

-- >>> runReturn inputFiles1 inputP solve1 1
-- "1-8_1a: 14 ,  1-8_1b: 295"

solve1 (rows, cols, xs) = uniqCount $ concatMap (filter inGrid . getAntinodes) $ groupAntennas xs
  where
    groupAntennas = map (map snd) . groupBy ((==) `on` fst) . sortBy (compare `on` fst)
    inGrid (y, x) = and [x >= 0, x < cols, y >= 0, y < rows]

-- inGrid (y, x) = x `elem` [0 .. cols - 1] && y `elem` [0 .. rows - 1]

getAntinodes xs = concatMap getAntinodePair pairs
  where
    pairs = [(a, b) | (a : ys) <- tails xs, b <- ys]
    getAntinodePair ((y1, x1), (y2, x2)) = [(y1 - dirY, x1 - dirX), (y2 + dirY, x2 + dirX)]
      where
        (dirY, dirX) = (y2 - y1, x2 - x1)

uniqCount = length . group . sort

-- >>> runReturn inputFiles1 inputP solve2 2
