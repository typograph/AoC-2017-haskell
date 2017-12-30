import Text.Megaparsec hiding (count)
import Text.Megaparsec.Char
import Data.Void (Void)
import Data.Maybe (fromMaybe)
import Data.Either (rights)
import Debug.Trace (traceShowId, trace)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.List (foldl')

type Parser a = Parsec Void String a
type Retriever = Char -> Int

data Command = Set Char (Retriever -> Int -> Int) -- Set register depending on its value
             | Jump (Retriever -> Bool) (Retriever -> Int) -- Jump if
--type Commands = Seq.Seq Command
type Commands = Seq.Seq (String, Command)

instance Show Command where
  show (Set c _) = "Set " ++ [c]
  show (Jump _ _) = "Jump"

number :: Parser Int
number = do op <- (char '-' >> return negate) <|> return id
            op . read <$> some digitChar

regOrNum :: Parser (Retriever -> Int)
regOrNum = flip ($) <$> lowerChar <|> const <$> number

{-}
gzCond :: Parser (Retriever -> Bool)
gzCond = (.) (>0) <$> regOrNum -}

nzCond :: Parser (Retriever -> Bool)
nzCond = (.) (/=0) <$> regOrNum

oneArgParser :: String -> Parser a -> Parser a
oneArgParser cmd regParser =
  do string cmd
     skipSome spaceChar
     regParser

twoArgsParser :: String -> Parser a -> Parser b -> Parser (a, b)
twoArgsParser cmd reg1Parser reg2Parser =
   do _ <- string cmd
      skipSome spaceChar
      reg1 <- reg1Parser
      skipSome spaceChar
      reg2 <- reg2Parser
--      many spaceChar
      return (reg1, reg2)

setParser :: String -> Parser (Char, Retriever -> Int)
setParser s = twoArgsParser s lowerChar regOrNum

commandParser :: Parser Command
commandParser = choice
  [ try $ uncurry Set <$> fmap (const .) <$> setParser "set"
  , try $ uncurry Set <$> fmap ((*).) <$> setParser "mul"
  , try $ uncurry Set <$> fmap (flip (-) .) <$> setParser "sub"
  , uncurry Jump <$> twoArgsParser "jnz" nzCond regOrNum
  ]

calculate :: Commands -> Int
calculate cmnds = calculate' Map.empty 0 0
  where retriever rs r = Map.findWithDefault 0 r rs
        calculate' regs ic mulcnt =
          if ic >= Seq.length cmnds then mulcnt else
          let retriever' = retriever regs
              (desc, command) = cmnds `Seq.index` ic
          in
          case command of
            Set c vfun -> let value = vfun retriever' $ retriever' c
                              newRegs = Map.insert c value regs
                          in if take 3 desc == "mul" then calculate' newRegs (ic+1) (mulcnt + 1)
                                                     else calculate' newRegs (ic+1) mulcnt
            Jump cond vfun -> let offset = if cond retriever'
                                           then vfun retriever'
                                           else 1
                              in calculate' regs (ic + offset) mulcnt

isNotPrime :: Int -> Bool
isNotPrime n = any ((==0) . mod n) [2 .. floor $ sqrt $ fromIntegral n]

count :: Foldable t => (a -> Bool) -> t a -> Int
count f = foldl' (\c a -> c + fromEnum (f a))  0

main :: IO ()
main = solution <$> readFile "data/advent_23.txt" >>= print
  where solution file =
          let fileParser =
                do cmnds <- sepEndBy1 commandParser space
                   eof
                   return cmnds
          in case parse fileParser "" file of
              Left e -> error $ parseErrorPretty e
              Right cmnds -> (calculate $ Seq.fromList $ zip (lines file) cmnds, count isNotPrime [106700, 106717 .. 123700] )
