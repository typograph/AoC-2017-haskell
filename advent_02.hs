spread :: [Int] -> Int
spread ints = maximum ints - minimum ints

wholeDiv :: [Int] -> Int
wholeDiv ints =
  let filterDiv i = filter (\(x,y) -> x /= 1 && y == 0) $ map (divMod i) ints
  in (fst . head . concatMap filterDiv) ints

-- main = show <$> checksum <$> readFile "data/advent_02.txt" >>= putStrLn

main :: IO ()
main = solution <$> readFile "data/advent_02.txt" >>= putStrLn
  where solution file =
          let ints =  map (map read . words) $ lines file
              checksum1 = sum $ map spread ints
              checksum2 = sum $ map wholeDiv ints
          in "Part I : " ++ show checksum1 ++ "\nPart II : " ++ show checksum2
