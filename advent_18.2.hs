import Text.Parsec hiding (State)
import Data.Either (rights)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

type Parser a = Parsec String () a
type Retriever = Char -> Int

data Command = Set Char (Retriever -> Int -> Int) -- Set register depending on its value
             | Jump (Retriever -> Bool) (Retriever -> Int) -- Jump if
             | Send (Retriever -> Int)
             | Receive Char
--type Commands = Seq.Seq Command
type Commands = Seq.Seq (String, Command)

data Status = Working | Receiving | Terminated deriving Show
data State = State Status Int [Int] (Map.Map Char Int) deriving Show

instance Show Command where
  show (Set c _) = "Set " ++ [c]
  show (Jump _ _) = "Jump"
  show (Send _) = "Send"
  show (Receive c) = "Receive " ++ [c]

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
  [ try $ Send <$> oneArgParser "snd" regOrNum
  , try $ Receive <$> oneArgParser "rcv" lower
  , try $ uncurry Set <$> fmap (const .) <$> setParser "set"
  , try $ uncurry Set <$> fmap ((*).) <$> setParser "mul"
  , try $ uncurry Set <$> fmap ((+).) <$> setParser "add"
  , try $ uncurry Set <$> fmap (flip mod .) <$> setParser "mod"
  , uncurry Jump <$> twoArgsParser "jgz" gzCond regOrNum
  ]

terminate :: State -> State
terminate s@(State Terminated _ _ _) = s
terminate (State _ i o r) = State Terminated i o r

clearStack :: State -> State
clearStack (State s i _ r) = State s i [] r

stepProgram :: Commands -> [Int] -> State -> State
stepProgram _ _ s@(State Terminated _ _ _) = s
stepProgram _ [] s@(State Receiving _ _ _) = s
stepProgram cmnds incoming (State Receiving ic outgoing regs) =
  stepProgram cmnds incoming $ State Working ic outgoing regs
stepProgram cmnds incoming (State Working ic outgoing regs) =
  if ic >= length cmnds then State Terminated ic outgoing regs else
  let retriever r = Map.findWithDefault 0 r regs
      (desc, command) = cmnds `Seq.index` ic
  in case command of
        Set c vfun -> let value = vfun retriever $ retriever c
                          newRegs = Map.insert c value regs
                      in stepProgram cmnds incoming $ State Working (ic+1) outgoing newRegs
        Jump cond vfun -> let offset = if cond retriever
                                       then vfun retriever
                                       else 1
                          in stepProgram cmnds incoming $ State Working (ic+offset) outgoing regs
        Send vfun -> stepProgram cmnds incoming $ State Working (ic+1) (vfun retriever : outgoing) regs
        Receive c -> case incoming of
                       [] -> State Receiving ic outgoing regs
                       v:vs -> let newRegs = Map.insert c v regs
                               in stepProgram cmnds vs $ State Working (ic+1) outgoing newRegs

calculate :: Commands -> Int
calculate cmnds = calculate' 0
                    (State Working 0 [] $ Map.singleton 'p' 0)
                    (State Working 0 [] $ Map.singleton 'p' 1)

  where calculate' sent (State Terminated _ _ _) (State Terminated _ out _) =
          sent + length out
        calculate' sent s0@(State Terminated _ [] _) s1@(State Receiving _ _ _) =
          calculate' sent s0 (terminate s1)
        calculate' sent (State Terminated i out r) s1@(State Receiving _ _ _) =
          calculate' sent (State Terminated i [] r) $ stepProgram cmnds (reverse out) s1
        calculate' sent s0@(State Terminated _ out _) s1@(State Working _ _ _) =
          calculate' sent (clearStack s0) $ stepProgram cmnds (reverse out) s1

        calculate' sent s0@(State Receiving _ _ _) s1@(State Terminated _ [] _) =
          calculate' sent (terminate s0) s1
        calculate' sent s0@(State Receiving _ [] _) s1@(State Receiving _ [] _) =
          calculate' sent (terminate s0) (terminate s1)
        calculate' sent s0@(State Receiving _ out _) s1@(State Receiving _ [] _) =
          calculate' sent (clearStack s0) $ stepProgram cmnds (reverse out) s1
        calculate' sent s0@(State Receiving _ out _) s1@(State Working _ [] _) =
          calculate' sent (clearStack s0) $ stepProgram cmnds (reverse out) s1
        calculate' sent s0@(State Receiving _ _ _) s1@(State _ _ out _) =
          calculate' (sent + length out) (stepProgram cmnds (reverse out) s0) (clearStack s1)

        calculate' sent s0@(State Working _ _ _) s1@(State _ _ out _) =
          calculate' (sent + length out) (stepProgram cmnds (reverse out) s0) (clearStack s1)

main :: IO ()
main = solution <$> readFile "data/advent_18.txt" >>= print
  where solution = calculate . Seq.fromList . rights . map (parse commandParser "") . lines
