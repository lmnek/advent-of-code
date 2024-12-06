module Day06 (main) where

import AOCUtils (runPrint, runReturn)
import Data.List
import Text.Parsec

inputFiles1 = ["6_1a", "6_1b"]

main :: IO ()
main = do
    runPrint inputFiles1 inputP (uncurry solve1) 1

-- runPrint inputFiles1 inputP solve2 2

-- CLASSES ----------------------------

type Cond = (Int, Int)

-- PARSING ----------------------------

numberP = read <$> many1 digit

condP = do
    a <- numberP
    char '|'
    b <- numberP
    endOfLine
    return (a, b)

updateP = numberP `sepBy1` char ','

inputP = do
    conds <- many1 condP
    endOfLine
    updates <- updateP `endBy1` endOfLine
    return (conds, updates)

-- >>> runReturn inputFiles1 inputP (uncurry solve1) 1
-- "1-5_1a: 143 ,  1-5_1b: 5268"

solve1 :: [Cond] -> [[Int]] -> Int
solve1 conds = sum . map midElem . filter (checkUpdate conds)

checkUpdate conds update = all (checkCondition (getIndexMap update)) conds

checkCondition indexMap (a, b) = case (indexMap !? a, indexMap !? b) of
    (Just i, Just j) -> i < j
    _ -> True

midElem xs = xs !! (length xs `div` 2)

getIndexMap update = Data.IntMap.fromList $ zip update [0 ..]

-- >>> runReturn inputFiles1 inputP (uncurry solve2) 2
-- "2-5_1a: 123 ,  2-5_1b: 5799"

{--
     I used topological sort; other approaches would include:
     - sortBy with custom sorting function based on conditions
     - no need for sorting when only getting mid element!
        -> just find elem. which has (length/2) of elems before and after ~ from conditions
--}

solve2 :: [Cond] -> [[Int]] -> Int
solve2 conds = sum . map (midElem . sortUpdate conds) . filter (not . checkUpdate conds)

-- NOTE: using fgl library would be simpler than graph
sortUpdate conds update = map (fst3 . nodeFromVertex) $ topSort graph
  where
    activeConds = filter (useCondition (getIndexMap update)) conds
    outEdges from = map snd $ filter ((== from) . fst) activeConds
    edgeList = map (\x -> (x, x, outEdges x)) update
    (graph, nodeFromVertex) = Graph.graphFromEdges' edgeList

useCondition indexMap (a, b) = case (indexMap !? a, indexMap !? b) of
    (Just _, Just _) -> True
    _ -> False

fst3 (a, _, _) = a
