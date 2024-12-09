module Day09 (main) where

import AOCUtils (runPrint, runReturn)
import Data.Function
import Data.List
import Text.Parsec
import Text.Parsec.String (Parser)

inputFiles1 = ["9_1a", "9_1b"]

main :: IO ()
main = do
    runPrint inputFiles1 inputP solve1 1

-- runPrint inputFiles1 inputP solve2 2

-- PARSING ----------------------------

numberP :: Parser Int
numberP = read . singleton <$> digit
inputP = many numberP

-- >>> runReturn inputFiles1 inputP solve1 1
-- "1-9_1a: 1928 ,  1-9_1b: 6225730762521"

solve1 xs = move 0 0 xsWithIndeces (reverse xsWithIndeces)
  where
    xsWithIndeces = zip [0 ..] xs

move idx acc ((_, 0) : curRest) lasts = move idx acc curRest lasts
move idx acc firsts ((_, 0) : lastRest) = move idx acc firsts lastRest
move idx acc cs@((curId, curSize) : curRest) ls@((lastId, lastSize) : lastRest)
    -- pointers from fron and back crossed -> moved all blocks -> end! (just add remaining blocks)
    | curId == lastId = foldr (\i acc' -> acc' + accInc (idx + i) curId) acc [0 .. min lastSize curSize - 1]
    -- skip moving empty space
    | odd lastId = move idx acc cs lastRest
    -- move last block to empty space
    | odd curId = move (idx + 1) (newAcc lastId) ((curId, curSize - 1) : curRest) ((lastId, lastSize - 1) : lastRest)
    -- on normal block - no moving
    | otherwise = move (idx + 1) (newAcc curId) ((curId, curSize - 1) : curRest) ls
  where
    accInc i id = i * (id `div` 2)
    newAcc id = acc + accInc idx id
