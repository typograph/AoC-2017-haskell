import Prelude hiding (Left, Right)
import qualified Data.Vector as Vec
import Data.Maybe (fromJust)
import Debug.Trace (traceShow)
--import Control.Applicative ((<|>))

type CMatrix = Vec.Vector (Vec.Vector Char)

data Direction = Left | Right | Up | Down deriving (Show, Eq)

type Point = (Int, Int, Direction)

move :: Point -> Point
move (y, x, Down) = (y+1, x, Down)
move (y, x, Up) = (y-1, x, Up)
move (y, x, Left) = (y, x-1, Left)
move (y, x, Right) = (y, x+1, Right)

trace :: CMatrix -> (Int, String)
trace field = reverse <$> trace' start 0 []
  where startx = fromJust $ Vec.findIndex (=='|') $ Vec.head field
        start = (0, startx, Down)
        trace' :: Point -> Int -> String -> (Int, String)
        trace' pt@(y, x, _) i s =
          let c = field Vec.! y Vec.! x
          in case traceShow pt c of
              '|' -> trace' (move pt) (i+1) s
              '-' -> trace' (move pt) (i+1) s
              '+' -> trace' (sidestep pt) (i+1) s
              ' ' -> (i, s)
              a -> trace' (move pt) (i+1) (a:s)
        sidestep :: Point -> Point
        sidestep (y, x, d) =
          let useIf :: Bool -> Point -> (Char -> Bool) -> Maybe Char -> Maybe Point
              useIf b pt fc e = if not b then Nothing else
                case e of
                  Nothing -> Nothing
                  Just c -> if fc c then Just pt else Nothing
              up = useIf (d /= Down) (y-1, x, Up) (/=' ') $ (Vec.! x) <$> field Vec.!? (y-1)
              down = useIf (d /= Up) (y+1, x, Down) (/=' ') $ (Vec.! x) <$> field Vec.!? (y+1)
              left = useIf (d /= Right) (y, x-1, Left) (/=' ') $ field Vec.! y Vec.!? (x-1)
              right = useIf (d /= Left) (y, x+1, Right) (/=' ') $ field Vec.! y Vec.!? (x+1)
              (<|>) :: Maybe a -> Maybe a -> Maybe a
              Nothing <|> a = a
              a <|> _ = a
          in fromJust $ up <|> down <|> left <|> right

main :: IO ()
main = solution <$> readFile "data/advent_19.test.txt" >>= print
  where solution = trace . Vec.fromList . map Vec.fromList . lines
