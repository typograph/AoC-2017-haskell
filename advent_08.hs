import Text.Parsec
import Data.Either (rights)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (foldl')

{-
b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10
-}

data Instruction = Instruction String (Int -> Int) String (Int -> Bool)

instance Show Instruction where
  show (Instruction rega op regc cond) = concat [rega, " if ", regc]

type Parser a = Parsec String () a

number :: Parser Int
number = do f <- (char '-' >> return negate) <|> return id
            n <- read <$> many1 digit
            return (f n)

arithmetical :: Parser (Int -> Int)
arithmetical =
  try ( do string "inc"
           spaces
           n <- number
           return (+n)
  ) <|> do string "dec"
           spaces
           n <- number
           return (subtract n)

conditional :: Parser (Int -> Bool)
conditional =
  do {
     cmpfun <- choice [ try $ string "!=" >> return (/=)
                      , try $ string "==" >> return (==)
                      , try $ string "<=" >> return (<=)
                      , try $ string "<"  >> return (<)
                      , try $ string ">=" >> return (>=)
                      , string ">"  >> return (>)
                      ] ;
     spaces ;
     n <- number ;
     return (`cmpfun` n) ;
     }

instruction :: Parser Instruction
instruction =
  do rega <- many1 lower
     many1 space
     arith <- arithmetical
     spaces
     string "if"
     many1 space
     regc <- many1 lower
     spaces
     cond <- conditional
     return $ Instruction rega arith regc cond

type RegMap = Map.Map String Int

processInstruction :: (Int, RegMap) -> Instruction -> (Int, RegMap)
processInstruction (x, m) (Instruction rega op regc cond) =
  let conditional = cond $ Map.findWithDefault 0 regc m
      value = op $ Map.findWithDefault 0 rega m
  in if conditional
     then (max value x, Map.insert rega value m)
     else (x, m)

process :: String -> Int
process s =
  let instructions = rights $ map (parse instruction "") $ lines s
      (vmax, registers) = foldl' processInstruction (0, Map.empty) instructions
  in vmax -- maximum registers -- Part 1

main = process <$> readFile "data/advent_08.txt" >>= print
