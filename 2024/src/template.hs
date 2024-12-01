import AOCUtils (run)
import Text.Parsec
import Text.Parsec.String (Parser)

inputFiles1 = [ "x_1" , "x_2" ]
inputFiles2 = inputFiles1

main :: IO() 
main = do 
    run inputFiles1 parseInput solve1 1 
    run inputFiles2 parseInput solve2 2 

-- DATA TYPES / CLASSES --------------


-- PARSING ----------------------------

parseLine = undefined

parseInput = undefined 

-- SOLUTION 1 -------------------------

--solve1 :: 
solve1 = undefined

-- SOLUTION 2 -------------------------

--solve2 :: 
solve2 = undefined

-- UTILS -------------------------


