import Text.Parsec
import Data.List (foldl')
import Data.Either (fromRight)
import Data.Bits (xor)
import Data.Char (ord, intToDigit, isSpace, isPrint)
import Data.Text (strip)

{- Part 1
lenList :: Parsec String () [Int]
lenList = sepBy number (char ',' >> spaces)
  where number = read <$> many1 digit
-}

{-
-- [from .. to], inclusive slice
slice :: Int -> Int -> [a] -> [a]
slice from to = drop from . take (to + from)
-}

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

intToHexString :: Int -> String
intToHexString n = [intToDigit $ div n 16, intToDigit $ mod n 16]

process str =
  let lengths = map ord (takeWhile isPrint $ dropWhile isSpace str) ++ [17, 31, 73, 47, 23]
      longHash = hash 64 256 lengths
      subHashes = map fst $ take 16 $ iterate (splitAt 16 . snd) (take 16 longHash, drop 16 longHash)
  in concatMap (intToHexString . foldl1 xor) subHashes

main = process <$> readFile "data/advent_10.txt" >>= putStrLn
--main = process <$> getLine >>= print
