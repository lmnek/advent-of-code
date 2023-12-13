import Data.List (find)
import Data.List.Extra (chunksOf)

import AOCUtils (run)
import Text.Parsec
import Text.Parsec.String (Parser)

inputFiles = [ "5_1" , "5_2" ]

main :: IO() 
main = do 
    run inputFiles parseInput1 solve1 1 
    run inputFiles parseInput2 solve2 2

-- DATA TYPES / CLASSES --------------

data Range = R Int Int Int deriving Show
data SeedRange = SR Int Int

-- PARSING ----------------------------

number = read <$> many1 digit

parseSeeds :: Parser [Int]
parseSeeds = string "seeds: " >> number `sepBy` char ' ' <* endOfLine

parseRange = do
    destinationStart <- number
    char ' '
    sourceStart <- number
    char ' '
    rangeLength <- number
    endOfLine
    return $ R destinationStart sourceStart rangeLength

parseMap = do
    source <- many1 letter 
    string "-to-"
    destination <- many1 letter
    string " map:\n"
    ranges <- many1 parseRange
    endOfLine
    return $ transform ranges 

parseInput1 = do
    seeds <- parseSeeds
    endOfLine
    transformations <- many1 parseMap 
    return (seeds, transformations)

parseInput2 = do
    (seeds, transformations) <- parseInput1
    let seedRanges = map (\sr -> SR (sr!!0) (sr!!1)) $ chunksOf 2 seeds
    return (seedRanges, transformations)

-- SOLUTION 1 -------------------------

solve1 :: ([Int], [(Int -> Int)]) -> Int
solve1 (seeds, maps) = minimum $ map getLocation seeds
    where getLocation seed = foldl (\prev mapTrans -> mapTrans prev) seed maps

transform :: [Range] -> Int -> Int
transform rs src = case find isForSrc rs of
        Nothing -> src
        Just (R d s _) -> d + (src - s)
    where isForSrc (R _ s len) = src >= s && src < s + len 
    

-- SOLUTION 2 -------------------------

-- TODO: brute-force too slow
solve2 :: ([SeedRange], [(Int -> Int)]) -> Int
solve2 (seedRanges, maps) = solve1 (seeds, maps)
    where allSeeds (SR str len) = [str..str+len-1] 
          seeds = concat $ map allSeeds seedRanges

