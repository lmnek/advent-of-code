import AOCUtils (run)
import Text.Parsec
import Text.Parsec.String (Parser)

inputFiles1 = [ "12_1" , "12_2" ]
inputFiles2 = inputFiles1

main :: IO() 
main = do 
    run inputFiles1 parseInput solve1 1 
    run inputFiles2 parseInput solve2 2 

-- DATA TYPES / CLASSES --------------


-- PARSING ----------------------------

number :: Parser Int
number = read <$> many1 digit

parseLine = do
    springs <- manyTill anyChar $ try (char ' ')
    sizes <- number `sepBy` char ','
    endOfLine
    return (springs, sizes)

parseInput = many1 parseLine

-- SOLUTION 1 -------------------------

--solve1 :: 
solve1 = sum . map numOfCombs 

numOfCombs (springs, sizes) = length $ combs springs sizes 0

combs :: String -> [Int] -> Int -> [String]
combs [] [] 0 = [""]
combs [] [size] curSize
    | curSize == size = [""]
    | otherwise = []
combs [] sizes curSize = []
combs (s:sx) sizes curSize 
    | s == '?' = combs ('#':sx) sizes curSize  
              ++ combs ('.':sx) sizes curSize 
    | s == '.' && curSize /= 0 = if sizes /= [] && curSize == (head sizes)
                                    then map (s:) $ combs sx (tail sizes) 0
                                    else []
    | otherwise = map (s:) $ combs sx sizes newCurSize
        where newCurSize = curSize + if s == '#' then 1 else 0

-- SOLUTION 2 -------------------------

-- TODO: too slow
solve2 rows = solve1 $ map unfold rows
    where unfold (springs, sizes) = (mulS springs 5, mul sizes 5)

mulS :: String -> Int -> String
mulS xs 1 = xs
mulS xs times = xs ++ "?" ++ mulS xs (times - 1)

mul :: [a] -> Int -> [a]
mul xs 1 = xs
mul xs times = xs ++ mul xs (times - 1)

-- UTILS -------------------------


