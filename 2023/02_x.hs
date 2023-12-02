import Text.Parsec
import Text.Parsec.String (Parser)
import Data.List (find)


main :: IO() 
main = do
    fileName <- getLine
    contents <- readFile $ "data/"++fileName
    doProblem contents parseGames solve1 1

doProblem :: Show b => String -> Parser a -> (a -> b) -> Int -> IO()
doProblem input parseData solve idx = do
    let res = parse parseData "" input 
    case res of
        Left err -> putStrLn $ "Parsing error: " ++ show err
        Right parsedData -> putStrLn $ show idx ++ ": " ++ solution 
            where solution = show $ solve parsedData

---------------------

-- Parsing

data Cube = Blue | Red | Green deriving (Show, Eq)
data Cubes = Cubes Int Cube deriving (Show, Eq)
type Set = [Cubes]
data Game = Game Int [Set] deriving Show

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
countInBag cube = maybe 0 (\(Cubes count _) -> count) cubesInBag 
    where cubesInBag = find (\(Cubes count cube') -> cube == cube') bag


solve2 :: [String] -> Int
solve2 a = 2
