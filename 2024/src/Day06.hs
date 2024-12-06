module Day06 (main) where

import AOCUtils (runPrint, runReturn)

-- import Data.Either (Left, Right)

import Data.Either
import Data.List
import Data.Maybe
import Text.Parsec

inputFiles1 = ["6_1a"] -- , "6_1b"]

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

objectP = do
    pos <- getPos
    c <- oneOf ".#^"
    return (c, pos)

inputP = do
    objectsPerLine <- many objectP `endBy1` endOfLine
    let size = (length objectsPerLine, length $ head objectsPerLine)
    let allObjects = concat objectsPerLine
    let obstacles = map snd $ filter ((== '#') . fst) allObjects
    let guard = snd . head $ filter ((== '^') . fst) allObjects
    return (size, guard, obstacles)

-- >>> runReturn [inputFiles1 !! 0] inputP parsedOutput 1
-- "1-6_1a: ([[('.',(0,0)),('.',(0,1)),('.',(0,2)),('.',(0,3)),('#',(0,4)),('.',(0,5)),('.',(0,6)),('.',(0,7)),('.',(0,8)),('.',(0,9))],[('.',(1,0)),('.',(1,1)),('.',(1,2)),('.',(1,3)),('.',(1,4)),('.',(1,5)),('.',(1,6)),('.',(1,7)),('.',(1,8)),('#',(1,9))],[('.',(2,0)),('.',(2,1)),('.',(2,2)),('.',(2,3)),('.',(2,4)),('.',(2,5)),('.',(2,6)),('.',(2,7)),('.',(2,8)),('.',(2,9))],[('.',(3,0)),('.',(3,1)),('#',(3,2)),('.',(3,3)),('.',(3,4)),('.',(3,5)),('.',(3,6)),('.',(3,7)),('.',(3,8)),('.',(3,9))],[('.',(4,0)),('.',(4,1)),('.',(4,2)),('.',(4,3)),('.',(4,4)),('.',(4,5)),('.',(4,6)),('#',(4,7)),('.',(4,8)),('.',(4,9))],[('.',(5,0)),('.',(5,1)),('.',(5,2)),('.',(5,3)),('.',(5,4)),('.',(5,5)),('.',(5,6)),('.',(5,7)),('.',(5,8)),('.',(5,9))],[('.',(6,0)),('#',(6,1)),('.',(6,2)),('.',(6,3)),('^',(6,4)),('.',(6,5)),('.',(6,6)),('.',(6,7)),('.',(6,8)),('.',(6,9))],[('.',(7,0)),('.',(7,1)),('.',(7,2)),('.',(7,3)),('.',(7,4)),('.',(7,5)),('.',(7,6)),('.',(7,7)),('#',(7,8)),('.',(7,9))],[('#',(8,0)),('.',(8,1)),('.',(8,2)),('.',(8,3)),('.',(8,4)),('.',(8,5)),('.',(8,6)),('.',(8,7)),('.',(8,8)),('.',(8,9))],[('.',(9,0)),('.',(9,1)),('.',(9,2)),('.',(9,3)),('.',(9,4)),('.',(9,5)),('#',(9,6)),('.',(9,7)),('.',(9,8)),('.',(9,9))]],(10,10),(6,4),[(0,4),(1,9),(3,2),(4,7),(6,1),(7,8),(8,0),(9,6)])"
parsedOutput = id

-- >>> runReturn inputFiles1 inputP solve1 1
-- "1-6_1a: [(0,4),(1,9),(3,2),(4,7),(6,1),(7,8),(8,0),(9,6)]"

-- solve1 (start, obs) = until
--     where
--         isOut =

nextDir U = R
nextDir R = D
nextDir D = L
nextDir L = U

-- >>> runReturn inputFiles1 inputP (uncurry solve2) 2
-- "2-5_1a: 123 ,  2-5_1b: 5799"

-- solve2 conds = sum . map (midElem . sortUpdate conds) . filter (not . checkUpdate conds)
