import Data.Char (isSpace, isPrint)
{-}
circ_zip :: [a] -> [(a,a)]
circ_zip [] = undefined   -- there was no specification for this case
circ_zip (x:[]) = undefined -- again, no specification
circ_zip (x:xs) = circ_zip_helper x (x:xs)
  where circ_zip_helper :: a -> [a] -> [(a,a)]
        circ_zip_helper _ [] = undefined
        circ_zip_helper z (x:[]) = (x,z) : []
        circ_zip_helper z (x:(y:ys)) = (x,y) : circ_zip_helper z (y:ys)
        -}

circZip :: [a] -> [(a,a)]
--circZip ls = zip ls (tail ls ++ [head ls]) -- Part 1
circZip ls =
  let nHalf = length ls `div` 2
      perm = drop nHalf ls ++ take nHalf ls
  in zip ls perm

count :: String -> Int
count = sum . map (\(a,b) -> if a == b then fromEnum a - fromEnum '0' else 0) . circZip

main = count <$> trunc <$> readFile "data/advent_01.txt" >>= print
  where trunc = takeWhile isPrint . dropWhile isSpace
