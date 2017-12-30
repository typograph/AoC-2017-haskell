import Text.Parsec
import Data.Maybe (fromMaybe)
import Data.Either (rights)
import Debug.Trace (traceShowId, trace)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

type Parser a = Parsec String () a
type Retriever = Char -> Int

data Command = Set Char (Retriever -> Int -> Int) -- Set register depending on its value
             | Jump (Retriever -> Bool) (Retriever -> Int) -- Jump if
             | Send Char
             | Receive (Retriever -> Bool)
--type Commands = Seq.Seq Command
type Commands = Seq.Seq (String, Command)

instance Show Command where
  show (Set c _) = "Set " ++ [c]
  show (Jump _ _) = "Jump"
  show (Send c) = "Send " ++ [c]
  show (Receive _) = "Receive"

number :: Parser Int
number = do op <- (char '-' >> return negate) <|> return id
            op <$> read <$> many1 digit

regOrNum :: Parser (Retriever -> Int)
regOrNum = flip ($) <$> lower <|> const <$> number

gzCond :: Parser (Retriever -> Bool)
gzCond = (.) (>0) <$> regOrNum

oneArgParser :: String -> Parser a -> Parser a
oneArgParser cmd regParser =
  do _ <- string cmd
     skipMany1 space
     reg <- regParser
     spaces
     eof
     return reg

twoArgsParser :: String -> Parser a -> Parser b -> Parser (a, b)
twoArgsParser cmd reg1Parser reg2Parser =
   do _ <- string cmd
      skipMany1 space
      reg1 <- reg1Parser
      skipMany1 space
      reg2 <- reg2Parser
      spaces
      eof
      return (reg1, reg2)

setParser :: String -> Parser (Char, Retriever -> Int)
setParser s = twoArgsParser s lower regOrNum

commandParser :: Parser Command
commandParser = choice
  [ try $ Send <$> oneArgParser "snd" lower
  , try $ Receive <$> oneArgParser "rcv" gzCond
  , try $ uncurry Set <$> fmap (const .) <$> setParser "set"
  , try $ uncurry Set <$> fmap ((*).) <$> setParser "mul"
  , try $ uncurry Set <$> fmap ((+).) <$> setParser "add"
  , try $ uncurry Set <$> fmap (flip mod .) <$> setParser "mod"
  , uncurry Jump <$> twoArgsParser "jgz" gzCond regOrNum
  ]

calculate :: Commands -> Int
calculate cmnds = calculate' Map.empty Nothing 0
  where retriever rs r = Map.findWithDefault 0 r rs
        calculate' regs freq ic =
          let retriever' = retriever regs
              (desc, command) = cmnds `Seq.index` ic
          in
          case trace desc command of
            Set c vfun -> let value = vfun retriever' $ retriever' c
                              newRegs = traceShowId $ Map.insert c value regs
                          in calculate' newRegs freq (ic+1)
            Jump cond vfun -> let offset = if cond retriever'
                                           then vfun retriever'
                                           else 1
                              in calculate' regs freq $ traceShowId (ic + offset)
            Send c -> calculate' regs (Just $ retriever' c) (ic+1)
            Receive cond -> if cond retriever'
                            then fromMaybe (error "No sound yet") freq
                            else calculate' regs freq (ic+1)

main :: IO ()
main = solution <$> readFile "data/advent_18.txt" >>= print
  where solution file =
          let commands = lines file
              parsed = zip commands $ rights $ map (parse commandParser "") commands
          in calculate $ Seq.fromList parsed
