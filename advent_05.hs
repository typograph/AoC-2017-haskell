import qualified Data.Sequence as Seq

type ISeq = Seq.Seq Int

stepEval :: (Int -> Int) -> (Int, ISeq) -> (Int, ISeq)
stepEval f (i,s) =
  let offset = Seq.index s i
      newOffset = f offset
  in (i + offset, Seq.update i newOffset s)

cntEval :: (Int -> Int) -> ISeq -> Int
cntEval f cmnds =
    let len = Seq.length cmnds
        cntEval' s state@(i,_) =
            if i < 0 || i+1 > len then s
            else cntEval' (s+1) $ stepEval f state
    in cntEval' 0 (0,cmnds)

main :: IO ()
main = solution <$> readFile "data/advent_05.txt" >>= putStrLn
  where solution file =
          let seq = Seq.fromList $ map read $ lines file
              advance1 offset = offset +1
              advance2 offset = if offset >= 3 then offset - 1 else offset + 1
          in "Part I : " ++ show (cntEval advance1 seq) ++ "\nPart II : " ++ show (cntEval advance2 seq)
