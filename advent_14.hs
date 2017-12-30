-- Code from Day 10
import Data.List (foldl')
import Data.Char (ord, intToDigit, isSpace, isPrint)
import Data.Bits (xor, popCount)
import qualified Data.Map as Map
import Debug.Trace

permute :: (Int, Int, Int, [Int]) -> Int -> (Int, Int, Int, [Int])
permute (i, skp, n, ls) len =
  let rolled = drop i ls ++ take i ls
      permuted = reverse (take len rolled) ++ drop len rolled
      unrolled = drop j permuted ++ take j permuted where j = n - i
  in ((i + len + skp) `mod` n, skp + 1, n, unrolled)

hash :: Int -> Int -> [Int] -> [Int]
hash cnt n = hash' cnt (0, 0, n, [0..n-1])
  where hash' 0 tpl _ = fourth tpl
        hash' c tpl lst = hash' (c-1) (foldl' permute tpl lst) lst
        fourth (_,_,_,d) = d

knotHash :: String -> Integer
knotHash str =
  let lengths = map ord (takeWhile isPrint $ dropWhile isSpace str) ++ [17, 31, 73, 47, 23]
      longHash = hash 64 256 lengths
      subHashes = map fst $ take 16 $ iterate (splitAt 16 . snd) $ splitAt 16 longHash
  in foldl' (\n d -> n*256 + fromIntegral d) (0::Integer) $ map (foldl1 xor) subHashes

knotHashLines :: String -> [Integer]
knotHashLines key = map (knotHash . ((key ++ "-") ++) . show) [0..127]

--

-- Remove cycles : k1->k2, k2->k3 => k1->k3, k2->k3
simplifyGraphMap :: (Ord k) => Map.Map k k -> Map.Map k k
simplifyGraphMap m =
  let mc = Map.filter (`Map.member` m) m
  in if Map.null mc then m
     else simplifyGraphMap $ Map.mapWithKey (\k v -> Map.findWithDefault v v m) m

cyclicLookup :: (Ord k) => k -> Map.Map k k -> k
cyclicLookup k m =
  case Map.lookup k m of
    Nothing -> k
    Just q -> cyclicLookup q m

countRegions :: [Integer] -> Int
countRegions grid = fromIntegral $ first $ foldl' markRegions (0, 1, replicate (length grid) 0) grid
  where first (a, _, _) = a
        markRegions (nr, nxt, lst) row =
          let combineRows (nr, nxt, row, ss@(s:_), mapping) above =
                let (rnxt, q) = divMod row 2 -- is the bit set?
                    a = cyclicLookup above mapping
                    sc = cyclicLookup s mapping
                in case q of
                    0 -> (nr, nxt, rnxt, 0:ss, mapping)
                    1 | s == 0 && above == 0 -> (nr+1, nxt+1, rnxt, nxt:ss, mapping)
                      | s == 0 && a /= 0 -> (nr, nxt, rnxt, a:ss, mapping)
                      | above == 0 -> (nr, nxt, rnxt, sc:ss, mapping)
                      | a == s -> (nr, nxt, rnxt, a:ss, mapping)
                      | otherwise -> (nr-1, nxt, rnxt, sc:ss, Map.insert a sc mapping)
              (nnr, nnxt, _, plst, m) = foldl' combineRows (nr, nxt, row, [0], Map.empty) lst
              mm = simplifyGraphMap m
              nlst = map (\k -> Map.findWithDefault k k mm) $ tail $ reverse plst
          in (nnr, nnxt, nlst)

main :: IO ()
main = print (countUsed hashGrid, countRegions hashGrid)
  where hashGrid = knotHashLines "jzgqcdpd"
        countUsed = sum . map popCount
