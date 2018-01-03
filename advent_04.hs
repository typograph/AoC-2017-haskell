import Data.List (sort, nub)

isValid :: [String] -> Bool
isValid wrds = length wrds == length (nub wrds)

main :: IO ()
main = solution <$> readFile "data/advent_04.txt" >>= putStrLn
  where solution file =
          let wrds = map words $ lines file
              swrds = map (map sort) wrds
              countValid = length . filter isValid
          in "Part I : " ++ show (countValid wrds) ++ "\nPart II : " ++ show (countValid swrds)