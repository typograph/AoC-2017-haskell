import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.Parsec
import Data.List (sort)
import Data.Either

data LevelData = LevelDesc String Int [String]
data LevelNode = Peak String Int | Level String Int [LevelNode]

level :: Parsec String () LevelData
level =
  let inBrackets = between (char '(') (char ')')
      integer = read <$> many1 digit
      identifier = many1 lower
  in do
    name <- identifier
    spaces
    weight <- inBrackets integer
    spaces
    leaves <- try (
      do spaces
         string "->"
         spaces
         identifier `sepBy1` (char ',' >> spaces)
      ) <|> return []
    return $ LevelDesc name weight leaves

addName :: String -> Set.Set String -> Set.Set String
addName name set
  | Set.member name set = Set.delete name set
  | otherwise = Set.insert name set

addTreeNode :: LevelData -> Set.Set String -> Set.Set String
addTreeNode (LevelDesc name weight names) set =
  foldr addName (addName name set) names

{- Part I
readTower :: String -> String
readTower file =
  let levels = map (parse level "") $ lines file
      lvlSet = foldr addTreeNode Set.empty levels
  in head $ Set.elems lvlSet
  -}

type LevelMap = Map.Map String (Int, [String])

mapTower :: [LevelData] -> LevelMap
mapTower = foldr ins Map.empty
  where ins (LevelDesc name weight children) = Map.insert name (weight, children)

collectLeaves :: LevelMap -> String -> LevelNode
collectLeaves m name =
  case Map.lookup name m of
    Nothing -> error "Node not in map"
    Just (w, children) -> case children of
      [] -> Peak name w
      _  -> Level name w $ map (collectLeaves m) children

fillTower :: [LevelData] -> LevelNode
fillTower levels =
  let topNode = head $ Set.elems $ foldr addTreeNode Set.empty levels
      lvlMap = mapTower levels
  in collectLeaves lvlMap topNode

findWrongWeight :: LevelNode -> Either Int (Int, Int)
findWrongWeight (Peak name w) = Right (w, w)
findWrongWeight (Level name w nodes) =
  let weights = sort $ map findWrongWeight nodes
  in case head weights of
    Left w1 -> Left w1
    Right (w1, we1) -> let (w2, we2) = fromRight undefined $ head $ tail weights
                       in if w1 /= w2 then Left (we1 + w2 - w1) -- What should the weight be?
                            else let (wn, wen) = fromRight undefined $ last weights
                              in if w2 /= wn then Left (wen + w2 - wn)
                                   else Right (w + w2 * length weights, w)

trackTower :: String -> Int
trackTower txt =
  let levels = rights $ map (parse level "") $ lines txt
      tree = fillTower levels
  in case findWrongWeight tree of
    Left w -> w
    Right _ -> undefined

main = show <$> trackTower <$> readFile "data/advent_07.txt" >>= putStrLn
