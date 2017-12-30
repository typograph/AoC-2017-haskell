import Text.Parsec
import Data.Either (rights)

penalty :: Int -> [(Int, Int)] -> Int
penalty offset = sum . map follow
  where follow :: (Int, Int) -> Int
        follow (i, d) =
          let pos' = mod (i+offset) $ 2*(d - 1)
          in if pos' == 0 then i*d else 0

caught :: [(Int, Int)] -> Int -> Bool
caught [] _ = False
caught ((i, d) : rs) offset =
    let pos' = mod (i+offset) $ 2*(d - 1)
    in pos' == 0 || caught rs offset

findOffset :: [(Int,Int)] -> Int
findOffset desc = head $ dropWhile (caught desc) [0..]

main :: IO ()
main = findOffset . parseLines <$> readFile "data/advent_13.txt" >>= print
  where parseLines = rights . map (parse lineParser "") . lines
        lineParser :: Parsec String () (Int, Int)
        lineParser =
          do pos <- read <$> many1 digit
             char ':' >> spaces
             depth <- read <$> many1 digit
             return (pos, depth)
