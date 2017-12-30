spread :: String -> Int
spread s = let ints = map read $ words s
           in maximum ints - minimum ints

wholeDiv :: String -> Int
wholeDiv s = let ints = map read $ words s
                 filterDiv i = filter (\(x,y) -> x /= 1 && y == 0) $ map (divMod i) ints
             in (fst . head . concatMap filterDiv) ints

checksum :: String -> Int
checksum = sum . map wholeDiv . lines

-- main = show <$> checksum <$> readFile "data/advent_02.txt" >>= putStrLn

main :: IO ()
main = do
  x <- readFile "data/advent_02.txt"
  print $ checksum x
