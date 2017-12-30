import qualified Data.Sequence as Seq
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Debug.Trace
import Data.List (maximumBy)

type Parser = Parsec Void String

data Piece = Piece Int Int

instance Eq Piece where
  Piece l1 r1 == Piece l2 r2 = (l1 == l2 && r1 == r2) || (l1 == r2 && r1 == l2)

instance Ord Piece where
  compare (Piece l1 r1) (Piece l2 r2)
    | m1 > m2 = GT
    | m1 < m2 = GT
    | otherwise = compare (min l1 r1) (min l2 r2)
    where m1 = max l1 r1
          m2 = max l2 r2

instance Show Piece where
  show (Piece l r) = show l ++ "/" ++ show r

type PSeq = Seq.Seq Piece

pieceFits :: Int -> Piece -> Bool
pieceFits i (Piece l r) = (l == i) || (r == i)

otherSide :: Int -> Piece -> Int
otherSide i (Piece l r)
  | l == i = r
  | r == i = l
  | otherwise = error "Wrong piece"

strength :: Piece -> Int
strength (Piece l r) = l+r


strongestBridge :: PSeq -> Int
strongestBridge = snd . sBridge' (0, 0, 0)
  where sBridge' b@(l, s, e) ps =
          let good = Seq.filter (pieceFits e) ps
              continueBridge p =
                sBridge' (l + 1, s + strength p, otherSide e p) $ Seq.filter (/= p) ps
              compareBridges b1@(l1,s1) b2@(l2,s2)
                  | l1 > l2 = GT
                  | l2 > l1 = LT
                  | s1 > s2 = GT
                  | s2 > s1 = LT
                  | otherwise = EQ
          in if Seq.null good then (l, s) else maximumBy compareBridges $ fmap continueBridge good


main :: IO ()
main = solution <$> readFile "data/advent_24.txt" >>= print
  where solution file =
          case parse (manyTill pieceParser eof) "" file of
            Left e -> error $ parseErrorPretty e
            Right pieces -> strongestBridge $ Seq.fromList pieces
        pieceParser :: Parser Piece
        pieceParser = do p1 <- read <$> some digitChar
                         _ <- char '/'
                         p2 <- read <$> some digitChar
                         space
                         return $ Piece p1 p2
