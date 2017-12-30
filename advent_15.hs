import Data.List (foldl')

generator :: Integer -> Integer -> Integer -> Integer
-- generator factor = (`mod` 2147483647) . (* factor) -- Part 1
generator divider factor prev =
  let nxt = mod (prev * factor) 2147483647
  in if nxt `mod` divider == 0 then nxt else generator divider factor nxt

countTies :: Int -> Int
countTies n = foldl' tie 0 $ take n $ tail $ iterate gen (512, 191) -- (65, 8921)
  where tie c (x,y) = if mod x 65536 == mod y 65536 then c+1 else c
        gen (x,y) = (genA x, genB y)
        genA = generator 4 16807
        genB = generator 8 48271

main :: IO ()
-- main = print $ countTies 40000000
main = print $ countTies 5000000
