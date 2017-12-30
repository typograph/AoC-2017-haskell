import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Data.List (foldl')
import Data.Either (fromRight)
import qualified Data.Map as Map
import Debug.Trace

data Move a = Spin Int | Exchange Int Int | Partner a a
data CompactMove a = CompactMove Int (Map.Map Int Int) (Map.Map a a) deriving (Show)
type Parser = Parsec Void String

moves :: Parser [Move Char]
moves = sepBy1 (choice [try spin, try exchange, partner]) (char ',' >> space)
  where spin = char 's' >> (Spin . read <$> some digitChar)
        exchange = do _ <- char 'x'
                      a <- read <$> some digitChar
                      _ <- char '/'
                      b <- read <$> some digitChar
                      return $ Exchange (min a b) (max a b)
        partner  = do _ <- char 'p'
                      a <- lowerChar
                      _ <- char '/'
                      b <- lowerChar
                      return $ Partner a b

move :: Eq a => [a] -> Move a -> [a]
move state (Spin n) = drop k state ++ take k state where k = length state - n
move state (Exchange n m) = -- Problem in case n/m-n-1 == length
  let (start, eln:end1) = splitAt n state
      (middle, elm:end) = splitAt (m-n-1) end1
  in start ++ [elm] ++ middle ++ [eln] ++ end
move state (Partner a b) = map swap state
  where swap e
          | e == a = b
          | e == b = a
          | otherwise = e

cmove :: (Eq a, Ord a) => [a] -> CompactMove a -> [a]
cmove state (CompactMove spin exch partn) =
  let spinned = move state $ Spin spin
      exchanged = map ((spinned !!) . (\x -> Map.findWithDefault x x exch)) [0..(length state - 1)]
      partnered = map (\x -> Map.findWithDefault x x partn) exchanged
  in partnered

compactMoves :: Int -> [Move Char] -> CompactMove Char
compactMoves len = foldr compactor $ CompactMove 0 Map.empty Map.empty
  where compactor :: Ord a => Move a -> CompactMove a -> CompactMove a
        compactor (Spin n) (CompactMove spin exch partn) =
          let newSpin = mod (spin + n) len
          in CompactMove newSpin exch partn
        compactor (Exchange i j) (CompactMove spin exch partn) =
          let newI = mod (i + spin) len
              newJ = mod (j + spin) len
              swap e
                | e == newI = newJ
                | e == newJ = newI
                | otherwise = e
              newExchange = Map.map swap $ Map.insertWith (flip const) newI newI $ Map.insertWith (flip const) newJ newJ exch
          in CompactMove spin newExchange partn
        compactor (Partner a b) (CompactMove spin exch partn) =
          let pA = Map.findWithDefault a a partn
              pB = Map.findWithDefault b b partn
              newPartner = Map.insert a pB $ Map.insert b pA partn
          in CompactMove spin exch newPartner

chainCompactMoves :: Ord a => Int -> CompactMove a -> CompactMove a -> CompactMove a
chainCompactMoves len (CompactMove s1 e1 p1) (CompactMove s2 e2 p2) =
  let newSpin = mod (s1 + s2) len
      re1 = Map.mapKeys ((`mod` len) . (+s2)) $ Map.map ((`mod` len) . (+s2)) e1
      newExchange = Map.union (Map.mapWithKey (\k v -> Map.findWithDefault v v re1) e2) re1
      newPartners = Map.union (Map.mapWithKey (\k v -> Map.findWithDefault v v p2) p1) p2
  in CompactMove newSpin newExchange newPartners

--

main :: IO ()
main = solution <$> readFile "data/advent_16.txt" >>= putStrLn
  where solution :: String -> String
        solution s =
          let initial = ['a'..'p']
              l = length initial
              cmt = compactMoves l $ fromRight [] $ parse moves "" s
          in "Part I : " ++ cmove initial cmt ++ "\nPart II: " ++ cmove initial (compactMul1e9 l cmt)
        compactMul10 l cmt =
          let c2 = chainCompactMoves l cmt cmt
              c4 = chainCompactMoves l c2 c2
              c8 = chainCompactMoves l c4 c4
          in chainCompactMoves l c2 c8
        compactMul1e3 l = compactMul10 l . compactMul10 l . compactMul10 l
        compactMul1e9 l = compactMul1e3 l . compactMul1e3 l . compactMul1e3 l
