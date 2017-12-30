import Text.Parsec
import Data.Either (fromRight)
import Data.List (foldl')

type Parser = Parsec String ()

{-
-- Part 1
garbage :: Parser (Int, Int)
garbage = between (char '<') (char '>') (many garbageContent) >> return (0, 0)
  where garbageContent = (char '!' >> anyChar) <|> noneOf ">"

group :: Parser (Int, Int)
group =
  let combine (a,x) (b,y) = (a+b, x+y)
      inc (ng, sm) = (ng + 1, sm + ng + 1)
      element = try group <|> garbage
      whole = between (char '{') (char '}') $ sepBy element (char ',')
  in inc . foldl' combine (0,0) <$> whole

main = (snd . fromRight (0,0) . parse group "") <$> readFile "data/advent_09.txt" >>= print
-}

-- Part 2
garbage :: Parser Int
garbage = sum <$> between (char '<') (char '>') (many garbageContent)
  where garbageContent = (char '!' >> anyChar >> return 0) <|> (noneOf ">" >> return 1)

group :: Parser Int
group =
  let element = try group <|> garbage
      whole = between (char '{') (char '}') $ sepBy element (char ',')
  in sum <$> whole

main = fromRight 0 . parse group "" <$> readFile "data/advent_09.txt" >>= print
