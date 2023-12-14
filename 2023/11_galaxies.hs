import AOCUtils (run)
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.List (tails, transpose)

inputFiles1 = [ "11_1", "11_2" ]
inputFiles2 = inputFiles1

main :: IO() 
main = do 
    run inputFiles1 parseInput solve 0

-- PARSING ----------------------------

parseLine =  many1 (noneOf "\n") <* endOfLine

parseInput = many1 parseLine

-- SOLUTIONS -------------------------

--expansion = 2 -- Part 1
expansion = 1000000 -- Part 2

solve universe = sum $ map distance' gsPairs
    where gsPairs = allPairs $ galaxies universe
          rowDists = prefixSum universe
          colDists = prefixSum $ transpose universe
          distance' = distance rowDists colDists

galaxies universe = [ (y, x) | (y, row) <- enum universe,
                      (x, c) <- enum row,
                      c == '#' ]

-- prefix sum of the rows distances
prefixSum :: [[Char]] -> [Int]
prefixSum xss = scanl (\prev xs -> prev + dist xs) 0 xss
    where dist row = if all (=='.') row then expansion else 1  

distance :: [Int] -> [Int] -> ((Int, Int), (Int, Int)) -> Int
distance rowD colD ((y1, x1), (y2, x2)) = yDiff + xDiff 
    where yDiff = abs $ (rowD !! y1) - (rowD !! y2)
          xDiff = abs $ (colD !! x1) - (colD !! x2)

-- UTILS -------------------------

enum = zip [0..]

allPairs :: [a] -> [(a, a)]
allPairs l = [(x,y) | (x:ys) <- tails l, y <- ys]
