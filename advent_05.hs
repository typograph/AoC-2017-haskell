import Data.Array.IO

cntEval :: (Int -> Int) -> IOUArray Int Int -> IO Int
cntEval f a = cntEval' 0 1
  where cntEval' :: Int -> Int -> IO Int
        cntEval' cnt ind =
          do inside <- (`inRange` ind) <$> getBounds a
             if inside
               then do v <- readArray a ind
                       writeArray a ind $ f v
                       cntEval' (cnt+1) (ind+v)
               else return cnt

main :: IO ()
main =
  do initState <- map read . lines <$> readFile "data/advent_05.txt"
     count1 <- cntEval (+1) =<< newListArray (1, length initState) initState
     count2 <- cntEval (\o -> if o>=3 then o-1 else o+1) =<< newListArray (1, length initState) initState
     putStrLn $ "Part I : " ++ show count1 ++ "\nPart II : " ++ show count2