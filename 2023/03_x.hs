import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative (some)
import Data.Char (isSymbol, isPunctuation)
import Data.List (find)

main :: IO() 
main = do
    fileName <- getLine
    input <- readFile $ "data/"++fileName
    doProblem input parseInput solve1 1
    doProblem input parseInput solve2 2

doProblem :: Show b => String -> Parser a -> (a -> b) -> Int -> IO()
doProblem input parseData solve idx = do
    let str = case parse parseData "" input  of
            Left err -> "Parsing error: " ++ show err
            Right parsedData -> show idx ++ ": " ++ solution 
                where solution = show $ solve parsedData
    putStrLn str

-- DATA TYPES / CLASSES --------------

data Pos = Pos Int Int deriving Show
data Numb = Numb { num :: Int, pos :: Pos } deriving Show
type Symb = Pos

-- PARSING ----------------------------

parseNumb :: Pos -> Parser (Either Numb Symb) 
parseNumb pos = do 
    num <- read <$> many1 digit
    return . Left $ Numb num pos 

symbol :: Parser Char
symbol = satisfy (\c -> (isSymbol c || isPunctuation c) 
    && not (c == '.'))

parseSymb :: Pos -> Parser (Either Numb Symb)
parseSymb pos = symbol >> return (Right pos)

parseVal :: Parser (Either Numb Symb)
parseVal = do
    posStart <- getPosition
    let pos = Pos (sourceColumn posStart - 1) (sourceLine posStart - 1)
    val <- parseNumb pos <|> parseSymb pos
    return val

dots = many $ char '.'
parseLine = dots >> parseVal `endBy` dots <* endOfLine

parseInput = do 
    vals <- concat <$> many parseLine
    let nx = [ n | Left n <- vals]
    let sx = [ s | Right s <- vals]
    return (nx, sx) 

-- SOLUTION 1 -------------------------

solve1 :: ([Numb], [Symb]) -> Int 
solve1 (nx, sx)= sum $ map toNum nx
    where toNum n@(Numb num _) = if isNumCloseToAny n sx 
                                        then num else 0

isNumCloseToAny :: Numb -> [Symb] -> Bool
isNumCloseToAny n sx = any isCloseToAny numPositions
    where numPositions = allPos n
          isCloseToAny p = any (isClose p) sx

isClose :: Pos -> Pos -> Bool
isClose (Pos x y) (Pos x' y') = (xd <= 1) && (yd <= 1) 
    where xd = abs $ x - x'
          yd = abs $ y - y'

allPos :: Numb -> [Pos]
allPos (Numb num (Pos x y)) = [(Pos x' y) | x' <- [x..lastPos]] 
        where numLen = length $ show num
              lastPos = x + numLen - 1

-- SOLUTION 2 -------------------------

solve2 :: ([Numb], [Symb]) -> Int
solve2 (nx, sx) = sum $ map (gearRatio nx) sx

gearRatio :: [Numb] -> Symb -> Int
gearRatio nx s = if length numsClose == 2
        then product numsClose
        else 0 
    where numsClose = map num $ filter (isCloseToNum s) nx

isCloseToNum :: Symb -> Numb -> Bool
isCloseToNum s n = any (isClose s) npx
    where npx = allPos n
