import AOCUtils (run)
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (forM_)

inputFiles1 = [ "10_1" , "10_2" ]
inputFiles2 = inputFiles1

main :: IO() 
main = do 
    run inputFiles1 parseInput solve1 1 

-- DATA TYPES / CLASSES --------------

data Pos = Pos { s :: Bool, n :: Bool, e :: Bool, w :: Bool } 
    deriving (Show, Eq)
type Idx = (Int, Int)

-- PARSING ----------------------------

parsePos = do 
    c <- anyChar
    let pos = case c of
            'S' -> Pos True True True True 
            '|' -> Pos True True False False
            '-' -> Pos False False True True 
            'L' -> Pos False True True False
            'J' -> Pos False True False True
            '7' -> Pos True False False True
            'F' -> Pos True False True False
            _   -> Pos False False False False 
    return pos


parseLine = manyTill parsePos newline
parseInput = many1 parseLine

-- SOLUTION 1 -------------------------

solve1 :: [[Pos]] -> Int
solve1 grid = (stepCount + 1) `div` 2
    where
        start = getStart grid
        traverse prev cur
            | cur == start = 0
            | otherwise = (+) 1 $ traverse cur $ next grid prev cur
        stepCount = traverse start (next grid (-1, -1) start)

next :: [[Pos]] -> Idx -> Idx -> Idx
next grid prev cur = case potentialPositions of
        [] -> error $ "Can't continue" 
        ps -> head ps -- should be alway only 1
    where potentialPositions = filter (isConnected grid cur)
            $ filter (withinBorders grid) 
            $ filter (/= prev) 
            $ map (app (+) cur) dirs

isConnected :: [[Pos]] -> Idx -> Idx -> Bool
isConnected grid cur pot = getCur (gridGet cur) && getPot (gridGet pot) 
    where gridGet (y, x) = (grid !! y) !! x
          diff = app (-) cur pot
          (getCur, getPot) = case diff of 
            (1, _)  -> (n, s)
            (-1, _) -> (s, n)
            (_, 1)  -> (w, e)
            (_, -1) -> (e, w)

withinBorders :: [[a]] -> Idx -> Bool
withinBorders grid (y, x) = y >= 0 && x >= 0 && y < length grid && x < length (grid !! 0)

getStart grid = [(x,y) | (x,row) <- zip [0..] grid, (y,val) <- zip [0..] row, 
    e val && w val && s val && n val] !! 0

dirs = [(1,0), (-1, 0), (0, 1), (0, -1)]

-- SOLUTION 2 -------------------------

-- TODO:
--solve2 :: 
solve2 = undefined

-- UTILS -------------------------

app :: (Int -> Int -> Int) -> Idx -> Idx -> Idx
app f (y, x) (y', x') = (f y y', f x x')
