-- import Data.Array
import qualified Data.Sequence as Seq

-- Given an array and an address

stepEval :: Int -> [Int] -> (Int, [Int])
stepEval 0 (x:xs) = (x, newOffset:xs)
  where newOffset = if x >= 3 then x-1 else x+1
stepEval n (x:xs)
  | n > 0 = let (n1, rst) = stepEval (n-1) xs in (n1 + 1, x:rst)
  | n < 0 = (n, x:xs)

type ISeq = Seq.Seq Int

stepEval' :: Int -> ISeq -> (Int, ISeq)
stepEval' i s =
  let offset = Seq.index s i
      newOffset = if offset >= 3 then offset - 1 else offset + 1
  in (i + offset, Seq.update i newOffset s)

cntEval :: ISeq -> Int
cntEval cmnds =
    let len = Seq.length cmnds
        cntEval' ln s i cs =
            if i < 0 || i+1 > ln
              then s
              else let (nxt, ccs) = stepEval' i cs
                   in cntEval' ln (s+1) nxt ccs
    in cntEval' len 0 0 cmnds

main :: IO ()
main = cntEval . Seq.fromList . map read . lines <$> readFile "data/advent_05.txt" >>= print
