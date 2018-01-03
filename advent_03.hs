import qualified Data.Map as Map

type PMap = Map.Map (Int, Int) Int

-- part I
manhattan :: Integral a => a -> a
manhattan n =
  let circle = ceiling $ (/2) $ sqrt (fromIntegral n) - 1
      sqside = 2*circle + 1
      radial_d = (sqside - 1) `div` 2
      tangent_d = abs $ mod (n - (sqside - 2) ^ 2) (sqside - 1) - circle
  in radial_d + tangent_d

-- part II
fillAt :: (Int, Int) -> PMap -> PMap
fillAt pt@(x, y) m = Map.insert pt value m
  where value = sum . map (\k -> Map.findWithDefault 0 k m) $ neighbours
        neighbours = [(x, y+1), (x+1, y+1), (x-1, y+1),
                      (x, y-1), (x+1, y-1), (x-1, y-1),
                      (x-1, y), (x+1, y)]

nextPt :: (Int, Int) -> (Int, Int)
nextPt (x, y)
  | x > 0 && abs y < x = (x, y+1)
  | y <= 0 && abs x <= -y = (x+1, y)
  | y > 0 && -x < y = (x-1, y)
  | x < 0 = (x, y-1)
  | otherwise = (0, 0)

ptGreater :: Int -> Int
ptGreater mx = third $ head $ dropWhile notReached $ iterate advance ((1,0), Map.singleton (0,0) 1, 1)
  where advance (pt, m, _) =
          let newMap = fillAt pt m
          in (nextPt pt, newMap, newMap Map.! pt)
        third (_, _, v) = v
        notReached = (<= mx) . third

main = putStrLn $ solution 368078
  where solution n = "Part I : " ++ show (manhattan n) ++ "\nPart II : " ++ show (ptGreater n)
