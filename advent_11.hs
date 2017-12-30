{-
 6 5 4 3 3 3 3 3 4 5
6 5 4 3 2 2 2 2 3 4 5
 5 4 3 2 1 1 1 2 3 4 5
5 4 3 2 1 0 1 2 3 4 5
-}

import Text.Parsec
import Data.Either (fromRight)
import Data.List (foldl')


nsteps :: Integral a => (a, a) -> a
nsteps (a,b) = max (abs a) (abs b)


motion :: String -> Int
motion = fst3 . foldl' calcMaxPos (0, 0, 0) . fromRight [(0,0)] . parse direction ""
  where direction :: Parsec String () [(Int,Int)]
        direction = flip sepBy1 (char ',' >> spaces) $
                     choice [ try $ string "nw" >> return (0,1)
                            , try $ string "ne" >> return (1,0)
                            , try $ string "n"  >> return (1,1)
                            , try $ string "se" >> return (0,-1)
                            , try $ string "sw" >> return (-1,0)
                            , string "s"  >> return (-1,-1) ]
        calcMaxPos :: (Int, Int, Int) -> (Int, Int) -> (Int, Int, Int)
        calcMaxPos (m, x, y) (sx, sy) = 
          let nx = x + sx
              ny = y + sy
          in (max m $ nsteps (nx, ny), nx, ny)
        fst3 (a,_,_) = a
  
main :: IO ()
main = motion <$> readFile "data/advent_11.txt" >>= print
