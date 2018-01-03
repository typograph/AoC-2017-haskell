import Data.Char (isSpace, isPrint)

circZip :: Int -> [a] -> [(a,a)]
circZip offset ls = zip ls $ drop offset ls ++ take offset ls

count :: Int -> String -> Int
count i = sum . map (\(a,b) -> if a == b then fromEnum a - fromEnum '0' else 0) . circZip i

main :: IO ()
main = solution <$> readFile "data/advent_01.txt" >>= putStrLn
  where solution file =
          let digits = takeWhile isPrint $ dropWhile isSpace file
              lHalf = length digits `div` 2
          in "Part I : " ++ show (count 1 digits) ++ "\nPart II : " ++ show (count lHalf digits)