import Prelude hiding (Left, Right)
import qualified Data.Either as E
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import qualified Data.Array.IArray as A
import qualified Data.Sequence as Seq
import Data.List (foldl')

type Parser = Parsec Void String

data Direction = Left | Right
data Move = Move Bool Direction Char

type BMove = Bool -> Move

type Rules = A.Array Char BMove

type Tape = Seq.Seq Bool
data TState = TState Char Int Tape
-- Parsers

buildBMove :: Move -> Move -> Bool -> Move
buildBMove mF mT b = if b then mT else mF

beginRule :: Parser Char
beginRule = between (string "Begin in state ") (char '.' >> space) upperChar

checksumRule :: Parser Int
checksumRule = between (string "Perform a diagnostic checksum after ") (string " steps." >> space) number
  where number = read <$> some digitChar

startProgramm :: Parser (Char, Int)
startProgramm =
  do state <- beginRule
     checksum <- checksumRule
     return (state, checksum)

stateCheckRule :: Parser Char
stateCheckRule = between (string "In state ") (char ':' >> space) upperChar

checkRule :: Bool -> Parser ()
checkRule t = space >> string "If the current value is " >> char (if t then '1' else '0') >> char ':' >> space

writeRule :: Parser Bool
writeRule = between (space >> string "- Write the value ") (char '.' >> space) zeroOne
  where zeroOne = (char '0' >> return False) <|> (char '1' >> return True)

moveRule :: Parser Direction
moveRule = between (space >> string "- Move one slot to the ") (char '.' >> space) leftOrRight
  where leftOrRight = try (string "left" >> return Left) <|> (string "right" >> return Right)

continueRule :: Parser Char
continueRule = between (space >> string "- Continue with state ") (char '.' >> space) upperChar

fullMoveRule :: Parser Move
fullMoveRule =
  do ruleW <- writeRule
     ruleD <- moveRule
     ruleS <- continueRule
     return $ Move ruleW ruleD ruleS

stateRule :: Parser (Char, BMove)
stateRule =
  do state <- stateCheckRule
     checkRule False
     ruleF <- fullMoveRule
     checkRule True
     ruleT <- fullMoveRule
     space
     return (state, buildBMove ruleF ruleT)

programmParser :: Parser (Char, Int, Rules)
programmParser =
  do (state, checksum) <- startProgramm
     rules <- some stateRule
     return (state, checksum, A.array ('A', fst $ last rules) rules)

--

stepTuring :: Rules -> TState -> TState
stepTuring r (TState s p t) =
  let Move w d ns = r A.! s $ Seq.index t p
      newTape = Seq.update p w t
  in case d of
      Left | p-1 >=0 -> TState ns (p-1) newTape
           | otherwise -> TState ns p (False Seq.<| newTape)
      Right | p+1 == Seq.length t -> TState ns (p+1) (newTape Seq.|> False)
            | otherwise -> TState ns (p+1) newTape

runTuring :: Char -> Int -> Rules -> Tape
runTuring state n rules = getTape $ (!! n) $ iterate (stepTuring rules) $ TState state 0 $ Seq.singleton False
  where  getTape (TState _ _ t) = t
--  where runTuring' 0 TState(_ _ tape) = tape
--        runTuring' n st = runTuring' (n-1) $ stepTuring rules st

calculateCheckSum :: Tape -> Int
calculateCheckSum = foldl' (\s b -> s + if b then 1 else 0) 0

--

main :: IO ()
main = solution <$> readFile "data/advent_25.txt" >>= print
  where solution file =
          case parse programmParser "" file of
            E.Left e -> error $ parseErrorPretty e
            E.Right (b, cs, rules) ->
              calculateCheckSum $ runTuring b cs rules
