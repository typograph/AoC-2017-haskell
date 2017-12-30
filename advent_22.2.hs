import Prelude hiding (Left, Right)
import qualified Data.Map as Map
import qualified Data.Either as E
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Data.List (foldl', intercalate)
import Data.Void (Void)
import Debug.Trace (traceShow, trace)
import System.Environment (getArgs)
--import Control.Applicative.Combinators (many1)

type Parser = Parsec Void String

data State = Clean | Weakened | Infected | Flagged
type PtMap = Map.Map (Int, Int) State
data Direction = Left | Right | Up | Down deriving (Show, Eq)

instance Enum Direction where
  succ Up = Right
  succ Right = Down
  succ Down = Left
  succ Left = Up

  pred Up = Left
  pred Left = Down
  pred Down = Right
  pred Right = Up

  toEnum 0 = Up
  toEnum 1 = Right
  toEnum 2 = Down
  toEnum 3 = Left

  fromEnum Up = 0
  fromEnum Right = 1
  fromEnum Down = 2
  fromEnum Left = 3

showGrid :: PtMap -> String
showGrid s = intercalate "\n" $ map makeRow [my, my-1 .. -my]
  where makeRow y = [ case Map.lookup (x,y) s of
                        Nothing -> '.'
                        Just Clean -> '.'
                        Just Weakened -> 'W'
                        Just Infected -> '#'
                        Just Flagged -> 'F'
                    | x <- [-mx .. mx]]
        (mx, my) = Map.foldlWithKey (\(x, y) (i, j) _ -> (max x $ abs i, max y $ abs j)) (0,0) s

takeSteps :: Int -> PtMap -> Int
takeSteps = takeSteps' 0 (0,0,Up)
  where takeSteps' ist _ 0 s = ist
        takeSteps' ist pt@(x,y,d) lft m =
          let inf = Map.lookup (x,y) m
              move fun (x,y,d) =
                case newd of
                  Left -> (x-1, y, newd)
                  Right -> (x+1, y, newd)
                  Up -> (x, y+1, newd)
                  Down -> (x, y-1, newd)
                where newd = fun d
          in case inf of
              Nothing -> takeSteps' ist (move pred pt) (lft-1) (Map.insert (x,y) Weakened m)
              Just Clean -> takeSteps' ist (move pred pt) (lft-1) (Map.insert (x,y) Weakened m)
              Just Weakened -> takeSteps' (ist+1) (move id pt) (lft-1) (Map.insert (x,y) Infected m)
              Just Infected -> takeSteps' ist (move succ pt) (lft-1) (Map.insert (x,y) Flagged m)
              Just Flagged -> takeSteps' ist (move (succ.succ) pt) (lft-1) (Map.delete (x,y) m)

main :: IO ()
main =
  do nsteps <- read . head <$> getArgs
     filecnt <- readFile "data/advent_22.txt"
     print $ takeSteps nsteps $ parse' filecnt
  where parse' file =
          case parse gridParser "" file of
            E.Left e -> error $ parseErrorPretty e
            E.Right grid ->
              let h = length grid
                  w = length $ head grid
                  foldRow y off r =
                    foldl' (\s (x, b) -> case b of { Clean -> s; b -> Map.insert (x, y) b s })
                      Map.empty
                      $ zip [-off..] r
                  foldGrid xoff yoff g =
                    foldl' (\s (y, r) -> Map.union s $ foldRow y xoff r)
                      Map.empty
                      $ zip [yoff,(yoff-1)..] g
              in foldGrid (div w 2) (div h 2) grid
        gridParser :: Parser [[State]]
        gridParser = sepEndBy1 (some pixel) (many spaceChar)
        pixel = (char '.' >> return Clean) <|> (char '#' >> return Infected)
