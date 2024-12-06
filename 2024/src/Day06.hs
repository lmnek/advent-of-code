module Day06 (main) where

import AOCUtils (runPrint, runReturn)

import Data.Either
import Data.Function
import Data.List hiding (insert)
import Data.Map (valid)
import Data.Maybe
import Data.Set as Set hiding (filter)
import Text.Parsec

inputFiles1 = ["6_1a", "6_1b"]

main :: IO ()
main = do
    runPrint inputFiles1 inputP solve1 1

-- runPrint inputFiles1 inputP solve2 2

-- CLASSES ----------------------------

type Pos = (Int, Int)

data Dir = U | D | R | L

-- PARSING ----------------------------

getPos = do
    posI <- getPosition
    return (sourceLine posI - 1, sourceColumn posI - 1)

validCharP = oneOf ".#^"

objectP = do
    pos <- getPos
    c <- validCharP
    return $ case c of
        '#' -> Just $ Left pos
        '^' -> Just $ Right pos
        _ -> Nothing

getColCountP = do
    many validCharP
    posI <- getPosition
    endOfLine
    return $ sourceColumn posI - 1

inputP = do
    colCount <- lookAhead getColCountP
    allObjects <- concat <$> (catMaybes <$> many objectP) `endBy1` endOfLine
    let obstacles = lefts allObjects
    let guard = head $ rights allObjects
    rowCount <- fst <$> getPos
    return ((rowCount, colCount), guard, obstacles)

-- >>> runReturn [inputFiles1 !! 0] inputP parsedOutput 1
-- "1-6_1a: ((10,10),(6,4),[(0,4),(1,9),(3,2),(4,7),(6,1),(7,8),(8,0),(9,6)])"

parsedOutput = id

-- >>> runReturn inputFiles1 inputP solve1 1
-- "1-6_1a: 42 ,  1-6_1b: 5312"

solve1 ((rows, cols), start, obs) = length visited
  where
    isOut (y, x) = x < 0 || y < 0 || y >= rows || x >= cols
    step' = step $ Set.fromList obs
    startAcc = (start, U, Set.empty)
    (_, _, visited) = until (isOut . fst3) step' startAcc

step obstacles (pos, dir, visited) =
    let
        newPos = move pos dir
        newDir =
            if move newPos dir `member` obstacles
                then rotateDir dir
                else dir
     in
        (newPos, newDir, newPos `insert` visited)

move (y, x) dir = (y + y_inc, x + x_inc)
  where
    (y_inc, x_inc) = case dir of
        U -> (-1, 0)
        D -> (1, 0)
        L -> (0, -1)
        R -> (0, 1)
rotateDir curDir = case curDir of
    U -> R
    R -> D
    D -> L
    L -> U

fst3 (a, _, _) = a

-- Much quicker version than going step by step -> rather jump to closest obstacle,
-- but takes too much time and logic to implement ... unfinished

-- todo: add hashset of visited states to accumalator
solve1b ((rows, cols), start, obs) = until (not . isOut . fst) (moveToObst obs) (start, U)
  where
    isOut (y, x) = x < 0 || y < 0 || y >= rows || x >= cols

-- todo: compute visited states
moveToObst obs (pos, dir) =
    let
        (constAxis, moveAxis, cmp, nextToTarget) = movementFuncs dir
        relevantObstacles = filter (cmp (moveAxis pos) . moveAxis) $ filter ((==) (constAxis pos) . constAxis) obs
     in
        case relevantObstacles of
            [] -> undefined -- todo: finish this branch till the border
            _ ->
                let
                    closestOb = minimumBy (compare `on` (abs . subtract (moveAxis pos) . moveAxis)) relevantObstacles
                    newPos = addT closestOb nextToTarget
                 in
                    (newPos, rotateDir dir)

movementFuncs curDir = case curDir of
    U -> (snd, fst, (>), (1, 0))
    D -> (snd, fst, (<), (-1, 0))
    R -> (fst, snd, (>), (0, -1))
    L -> (fst, snd, (<), (0, 1))
addT (a, b) (a', b') = (a + a', b + b')

-- >>> runReturn inputFiles1 inputP (uncurry solve2) 2
-- "2-5_1a: 123 ,  2-5_1b: 5799"

-- solve2 conds = sum . map (midElem . sortUpdate conds) . filter (not . checkUpdate conds)
--
--
