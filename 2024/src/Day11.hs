module Day11 (main) where

import AOCUtils (runPrint, runReturn)
import Data.Foldable
import Data.IntMap (empty, (!))
import qualified Data.IntMap as Map
import Data.IntMap.Strict (IntMap)
import Data.Tuple.Extra
import Text.Parsec
import Text.Parsec.String (Parser)

inputFiles = ["11_1", "11_2"]

main :: IO ()
main = do
    runPrint inputFiles inputP solve1 1

numberP :: Parser Int
numberP = read <$> many1 digit

inputP = numberP `endBy1` space

-- >>> runReturn inputFiles inputP solve1 1
-- "1-11_1: 55312 ,  1-11_2: 217443"
solve1 = solve True 25

-- >>> runReturn [inputFiles !!0] inputP solve2 2
solve2 = solve True 75

-- >>> Map.assocs $ buildMap 75
-- [(1,1),(2,1),(3,2),(4,4),(5,4),(6,7),(7,14),(8,16),(9,20),(10,39),(11,62),(12,81),(13,110),(14,200),(15,328),(16,418),(17,667),(18,1059),(19,1546),(20,2377),(21,3572),(22,5602),(23,8268),(24,12343),(25,19778)]

solve memoOn maxDepth = sum . map (stoneCount memoOn maxDepth zeroMap 0)
  where
    zeroMap = buildMap maxDepth

buildMap maxDepth = foldl iterDepth (Map.singleton 1 1) [1 .. maxDepth]
  where
    iterDepth zeroMap depth = Map.insert depth (stoneCount True (depth - 1) zeroMap 0 1) zeroMap

stoneCount :: Bool -> Int -> IntMap Int -> Int -> Int -> Int
stoneCount memoOn maxDepth zeroMap depth num
    | maxDepth == depth = 1
    | num == 0 =
        if memoOn
            then zeroMap ! (maxDepth - depth)
            else goDeeper 1
    | even digitCount =
        let
            (leftNum, rightNum) = both read $ splitAt (digitCount `div` 2) numStr
         in
            goDeeper leftNum + goDeeper rightNum
    | otherwise = goDeeper $ num * 2024
  where
    numStr = show num
    digitCount = length numStr
    goDeeper = stoneCount memoOn maxDepth zeroMap (depth + 1)
