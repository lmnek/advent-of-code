import AOCUtils (run)
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Map (Map, (!), fromList)

inputFiles1 = [ "8_1", "8_1b", "8_2" ]
inputFiles2 = [ "8_1c", "8_2" ]

main :: IO() 
main = do 
    run inputFiles1 parseInput1 solve1 1

-- CLASSES ----------------------------

type NodeMap = Map String (String, String)

-- PARSING ----------------------------

parseNode = many1 $ noneOf " ,)"

parseLine = do
    name <- parseNode
    string " = ("
    left <- parseNode
    string ", "
    right <- parseNode
    char ')'
    endOfLine
    return (name, (left, right)) 

parseInput1 = do
    instructions <- many1 $ noneOf "\n"
    endOfLine
    endOfLine
    nodeMap <- fromList <$> many1 parseLine 
    return (instructions, nodeMap)
    

parseInput2 = undefined

-- SOLUTION 1 -------------------------


solve1 :: (String, NodeMap) -> Int
solve1 (inss, nodeMap) = recursiveFoldl "AAA" 
    where 
          move (i, node) ins = let (left, right) = nodeMap ! node in
                                (i + 1, case ins of 
                                    'R' -> right
                                    'L' -> left)
          recursiveFoldl start
            | start == "ZZZ" = 0
            | otherwise = let (steps, end) = foldl move (0, start) inss 
                          in steps + recursiveFoldl end

-- SOLUTION 2 -------------------------

-- TODO: .... LCM cycle detection
--solve2 :: (String, NodeMap) -> Int
--solve2 a = 2


-- UTILS ------------------------------

