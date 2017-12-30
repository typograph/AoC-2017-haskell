import Text.Parsec
import Data.Either (rights)
import qualified Data.IntSet as Set
import qualified Data.Vector as Vec

type ISet = Set.IntSet
type AVec = Vec.Vector [Int]

countAt :: Int -> AVec -> Int
countAt i = Set.size . groupAt i

groupAt :: Int -> AVec -> ISet
groupAt i info = collect' (info Vec.! i) (Set.singleton i)
  where collect' :: [Int] -> ISet -> ISet
        collect' [] set = set
        collect' (e:es) set
          | Set.member e set = collect' es set
          | otherwise = let newBoundary = (info Vec.! e) ++ es
                            newSet = Set.insert e set
                        in collect' newBoundary newSet

countGroups :: AVec -> Int
countGroups info = countGroups' 0 []
  where countGroups' :: Int -> [ISet] -> Int
        countGroups' i sets
          | i == Vec.length info = length sets
          | otherwise = let newSets = groupAt i info : sets
                        in countGroups' (findNext newSets) newSets
        findNext :: [ISet] -> Int
        findNext s = findNext' 1 s s
        findNext' i _ [] = i
        findNext' i sets (s:ss)
          | Set.member i s = findNext' (i+1) sets sets
          | otherwise = findNext' i sets ss

main :: IO ()
main = solution <$> readFile "data/advent_12.txt" >>= print
  where solution = countGroups . Vec.fromList . rights . map (parse pipe "") . lines
        pipe :: Parsec String () [Int]
        pipe = do many1 digit
                  spaces
                  string "<->"
                  spaces
                  sepBy (read <$> many1 digit) (char ',' >> spaces)
