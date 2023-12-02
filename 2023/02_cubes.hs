import Text.Parsec
import Text.Parsec.String (Parser)
import Data.List (find)
import Data.List.Extra (groupOn, sortOn)


main :: IO() 
main = do
    fileName <- getLine
    contents <- readFile $ "data/"++fileName
    doProblem contents parseGames solve1 1
    doProblem contents parseGames solve2 2

doProblem :: Show b => String -> Parser a -> (a -> b) -> Int -> IO()
doProblem input parseData solve idx = do
    let str = case parse parseData "" input  of
            Left err -> "Parsing error: " ++ show err
            Right parsedData -> show idx ++ ": " ++ solution 
                where solution = show $ solve parsedData
    putStrLn str

---------------------

-- Parsing

data Cube = Blue | Red | Green deriving (Show, Eq, Ord)
data Cubes = Cubes { took :: Int, cube :: Cube } deriving (Show, Eq)
type Set = [Cubes]
data Game = Game { index :: Int, sets :: [Set] } deriving Show

parseInt = read <$> many1 digit 

parseCube :: Parser Cube
parseCube = (string "blue" >> return Blue)
    <|> (string "red" >> return Red)
    <|> (string "green" >> return Green)

parseCubes = do 
    took <- parseInt 
    space
    cube <- parseCube
    return $ Cubes took cube

parseSet = parseCubes `sepBy` (string ", ")

parseGame = do
    string "Game "
    gameIdx <- parseInt
    string ": "
    sets <- parseSet `sepBy` (string "; ")
    return $ Game gameIdx sets

parseGames = parseGame `endBy` (char '\n')

-- ------------

-- note: "foldl (&&) True $ map condition list" 
-- --> same as "all condition list"

bag = [Cubes 12 Red, Cubes 13 Green, Cubes 14 Blue]

solve1 :: [Game] -> Int 
solve1 = sum . map gameResult  

gameResult :: Game -> Int 
gameResult (Game i sets) = if all (all inBag) sets then i 
                           else 0

inBag :: Cubes -> Bool
inBag (Cubes took cube) = countInBag cube >= took

countInBag :: Cube -> Int
countInBag cubeType = maybe 0 took $ find ((== cubeType) . cube) bag 

-- ----------------------

-- note: groupOn func = groupBy ((==) `on` func)

solve2 :: [Game] -> Int
solve2 = sum . map power

power :: Game -> Int
power = product . (map minCubes) . group . sets

minCubes :: [Cubes] -> Int
minCubes = maximum . map took

group :: [Set] -> [[Cubes]]
group = groupOn cube . sortOn cube . concat


