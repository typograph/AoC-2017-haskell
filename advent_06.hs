import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

imax :: (Foldable t, Ord a) => t a -> (Int, a)
imax = coerce . foldl reductor (-1, 0, undefined)
  where reductor (im, iv, m) v
          | im == -1 = (iv, iv+1, v)
          | v > m = (iv, iv+1, v)
          | otherwise = (im, iv+1, m)
        coerce (im, iv, m) = (im, m)

redistribute :: Int -> Int -> Int -> Int -> [Int] -> [Int]
redistribute _ _ _ _ [] = []
redistribute m im len ix (x:xs) =
  let newx | ix == im  =  m `div` len
           | (ix - im) `mod` len <= m `mod` len  =  x + m `div` len + 1
           | otherwise  =  x + m `div` len
  in newx : redistribute m im len (ix + 1) xs


cycleCounter :: Int -> Int -> [[Int]] -> (Int, Int)
cycleCounter len ic cs@(c:_) =
  let (im, mx) = imax c
      newc = redistribute mx im len 0 c
  in case elemIndex newc cs of
      Just i -> (ic + 1, i + 1)
      Nothing -> cycleCounter len (ic+1) (newc : cs)


cycled :: [Int] -> Int
cycled cs =
  let len = length cs
  in snd $ cycleCounter len 0 [cs]

main = show . cycled . map read . words <$> readFile "data/advent_06.txt" >>= putStrLn
