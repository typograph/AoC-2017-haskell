import Data.List

isValid :: String -> Bool
isValid s =
  let wrds = words s
  in length wrds == length (nub wrds)

isValid2 :: String -> Bool
isValid2 s =
  let wrds = words s
  in length wrds == length (nub $ map sort wrds)

main = show <$> length <$> filter isValid2 <$> lines <$> readFile "data/advent_04.txt" >>= putStrLn
