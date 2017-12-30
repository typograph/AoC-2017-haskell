import Prelude hiding (Left, Right)
import qualified Data.Set as Set
import qualified Data.Either as E
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.List (foldl', intercalate)
import Data.Void (Void)
import Debug.Trace (traceShow, trace)
import System.Environment (getArgs)
--import Control.Applicative.Combinators (many1)

type Parser = Parsec Void String

type PtSet = Set.Set (Int, Int)
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

showGrid :: PtSet -> String
showGrid s = intercalate "\n" $ map makeRow [my, my-1 .. -my]
  where makeRow y = [ if Set.member (x,y) s then '#' else '.' | x <- [-mx .. mx]]
        (mx, my) = foldl' (\(x, y) (i, j) -> (max x $ abs i, max y $ abs j)) (0,0) s

takeSteps :: Int -> PtSet -> Int
takeSteps = takeSteps' 0 (0,0,Up)
  where takeSteps' ist _ 0 s = ist
        takeSteps' ist pt@(x,y,d) lft set =
          let inf = Set.member (x,y) set
              move fun (x,y,d) =
                case newd of
                  Left -> (x-1, y, newd)
                  Right -> (x+1, y, newd)
                  Up -> (x, y+1, newd)
                  Down -> (x, y-1, newd)
                where newd = fun d
          in if inf
              then takeSteps' ist (move succ pt) (lft-1) (Set.delete (x,y) set)
              else takeSteps' (ist+1) (move pred pt) (lft-1) (Set.insert (x,y) set)

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
                    foldl' (\s (x, b) -> if b then Set.insert (x, y) s else s)
                      Set.empty
                      $ zip [-off..] r
                  foldGrid xoff yoff g =
                    foldl' (\s (y, r) -> Set.union s $ foldRow y xoff r)
                      Set.empty
                      $ zip [yoff,(yoff-1)..] g
              in foldGrid (div w 2) (div h 2) grid
        gridParser :: Parser [[Bool]]
        gridParser = sepEndBy1 (some pixel) (many spaceChar)
        pixel = (char '.' >> return False) <|> (char '#' >> return True)
