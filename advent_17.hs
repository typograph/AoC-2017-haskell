import qualified Data.Sequence as Seq
import Debug.Trace
import Data.List (foldl')

type ISeq = Seq.Seq Int

-- This is step i, on which we insert i into the Sequence
-- The last step to take is n
-- The new value is inserted at
insN :: Int -> Int -> Int -> ISeq -> Int
insN step pos n sq
  | i > n = sq `Seq.index` (pos + 1) `mod` i
  | otherwise = insN step newPos n $ Seq.insertAt newPos i sq
  where i = Seq.length sq
        newPos = (pos + step) `mod` i + 1

solution1 :: Int -> Int -> Int
solution1 n step = insN step 0 n $ Seq.singleton 0

solution2 :: Int -> Int -> Int
solution2 n step = snd $ foldl' trackAt0 (1, 0) [1..n]
  where trackAt0 (p, r0) i =
          let pn = mod (p+step) i
          in (pn+1, if pn == 0 then i else r0)

-- This overflows the stack. Why?

main :: IO ()
main = print (solution1 2017 349, solution2 50000000 349)
