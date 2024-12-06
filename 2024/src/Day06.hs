module Day06 (main) where

import AOCUtils (runPrint, runReturn)

import Data.Either
import Data.Function
import Data.List hiding (insert)
import qualified Data.Map as Map
import Data.Map.Strict (fromList, valid, (!))
import Data.Maybe
import Data.Set as Set hiding (filter, foldr)
import Text.Parsec

inputFiles1 = ["6_1a", "6_1b"]

main :: IO ()
main = do
    runPrint inputFiles1 inputP solve1 1

-- runPrint inputFiles1 inputP solve2 2

-- CLASSES ----------------------------

type Pos = (Int, Int)

data Dir = U | D | R | L
    deriving (Eq, Show, Ord)

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

-- NOTE: could nicely refactor
-- data GuardState = GuardState
--     { pos :: Pos
--     , dir :: Dir
--     , visited :: [Pos] -- Tracking visited positions as a list for simplicity
--     , loopMap :: [(Dir, [Pos])] -- A list of directions and positions
--     , possibleObs :: [Pos] -- List of positions where a new obstacle could be placed
--     }

-- NOTE: Very nice efficient idea for part2 but does not work on large input :(

-- >>> runReturn inputFiles1 inputP solve2 2
-- "2-6_1a: 6 ,  2-6_1b: 446"

-- >>> runReturn [inputFiles1 !! 0] inputP solve2 2
-- "2-6_1a: 6"

solve2 ((rows, cols), start, obs) = length $ getPossibleObs out
  where
    isOut (y, x) = x < 0 || y < 0 || y >= rows || x >= cols
    step' = step2 isOut $ Set.fromList obs
    loopMap = Map.fromList $ [(dir, Set.empty) | dir <- [U, D, L, R]]
    startAcc = (start, U, Set.empty, loopMap, Set.empty)
    out = until (isOut . fst5) step' startAcc

type StepAcc = (Pos, Dir, Set Pos, Map.Map Dir (Set Pos), Set Pos)
step2 :: (Pos -> Bool) -> Set Pos -> StepAcc -> StepAcc
step2 isOut obstacles (pos, dir, visited, loopMap, possibleObs)
    -- bumped into box -> rotate + save segment into loopMap
    | isInObstacles newNextPos =
        let
            -- saving a segment, where loop could be found in the future
            moveOposite = flip move $ opositeDir dir
            getPossUntilBorder pos'
                | isInObstacles pos' || isOut pos' = []
                | otherwise = pos' : getPossUntilBorder (moveOposite pos')
            loopDir = prevDir dir
            newLoopDirSet = foldr Set.insert (loopMap ! loopDir) (getPossUntilBorder newPos)
            newLoopMap = Map.insert loopDir newLoopDirSet loopMap
         in
            (newPos, rotateDir dir, newVisited, newLoopMap, possibleObs)
    -- found a loop! ~ place to put new obstruction
    | newPos `member` (loopMap ! dir) =
        (newPos, dir, newVisited, loopMap, newNextPos `insert` possibleObs)
    | otherwise = (newPos, dir, newVisited, loopMap, possibleObs)
  where
    newPos = move pos dir
    newVisited = newPos `insert` visited
    newNextPos = move newPos dir
    isInObstacles ps = ps `member` obstacles

opositeDir curDir = case curDir of
    U -> D
    R -> L
    D -> U
    L -> R
prevDir curDir = case curDir of
    U -> L
    R -> U
    L -> D
    D -> R

getPossibleObs (_, _, _, _, obs) = obs
fst5 (x, _, _, _, _) = x

------------------------------------------------

-- Much quicker part1 version than going step by step -> rather jump to closest obstacle,
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
