{-# LANGUAGE MultiWayIf #-}

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error
import Data.Void (Void)
import Data.Either (lefts, rights)
import Data.List (minimumBy, sortBy, groupBy)

import Debug.Trace

import qualified Data.Map as Map
import qualified Data.Set as Set

type Parser a = Parsec Void String a

data Vec3 = Vec3 Int Int Int deriving (Show)
data Particle = Particle Vec3 Vec3 Vec3 deriving (Show)
data CollisionTime = Never | Once Int | Twice Int Int | Always deriving (Eq, Show)

instance Ord CollisionTime where
  compare Never  Never  = EQ
  compare Never  _      = GT
  compare _      Never  = LT
  compare Always Always = EQ
  compare Always _      = LT
  compare _      Always = GT
  compare (Once a) (Once b) = compare a b
  compare (Once a) (Twice b c) = compare a b
  compare (Twice a b) (Once c) = compare a c
  compare (Twice a b) (Twice c d) = compare a c

instance Eq Vec3 where
  (Vec3 x1 y1 z1) == (Vec3 x2 y2 z2) =
    (abs x1 + abs y1 + abs z1) == (abs x2 + abs y2 + abs z2)

instance Ord Vec3 where
  compare (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
    compare (abs x1 + abs y1 + abs z1) (abs x2 + abs y2 + abs z2)

number :: Parser Int
number = do op <- (char '-' >> return negate) <|> return id
            op . read <$> some digitChar

vector :: Parser Vec3
vector = do x <- number
            char ','
            y <- number
            char ','
            z <- number
            return $ Vec3 x y z

particle :: Parser Particle
particle =
  do string "p="
     pos <- between (char '<') (char '>') vector
     char ',' >> space
     string "v="
     vel <- between (char '<') (char '>') vector
     char ',' >> space
     string "a="
     acc <- between (char '<') (char '>') vector
     space
     eof
     return $ Particle pos vel acc

collision1D :: (Int, Int, Int) -> (Int, Int, Int) -> CollisionTime
collision1D (x1, v1, a1) (x2, v2, a2) =
  if v1 == v2 && a1 == a2 then if x1 == x2 then Always else Never
    else let det = (2*(v1-v2) + (a1-a2)) ^ 2 - 8*(x1-x2)*(a1-a2)
             sqdet = ceiling $ sqrt $ fromIntegral det
         in if | a1 == a2 -> filterTimes [fromIntegral (x2-x1) / fromIntegral(v1-v2)]
               | det < 0 -> Never
               | det /= sqdet ^ 2 -> Never
               | otherwise -> filterTimes [-0.5 - fromIntegral (2*(v1-v2) + sqdet) / fromIntegral (a1-a2) / 2,
                                           -0.5 -fromIntegral (2*(v1-v2) - sqdet) / fromIntegral (a1-a2) / 2]
               where filterTimes = convert . foldr fltTimes []
                     fltTimes a [] = [ceiling a | a >= 0 && ceiling a == floor a]
                     fltTimes a bs@(b:_) = if a >=0 && ceiling a == floor a then ceiling a : bs else bs
                     convert [] = Never
                     convert [a] = Once a
                     convert [a,b] = if a > b then Twice b a else Twice a b
                     convert _ = undefined

collisionTime :: Particle -> Particle -> CollisionTime
collisionTime (Particle (Vec3 rx1 ry1 rz1) (Vec3 vx1 vy1 vz1) (Vec3 ax1 ay1 az1))
              (Particle (Vec3 rx2 ry2 rz2) (Vec3 vx2 vy2 vz2) (Vec3 ax2 ay2 az2)) =
                let tx = collision1D (rx1, vx1, ax1) (rx2, vx2, ax2)
                    ty = collision1D (ry1, vy1, ay1) (ry2, vy2, ay2)
                    tz = collision1D (rz1, vz1, az1) (rz2, vz2, az2)
                in combineTimes tx $ combineTimes ty tz
                where combineTimes Never _ = Never
                      combineTimes _ Never = Never
                      combineTimes Always Always = Always
                      combineTimes Always b = b
                      combineTimes a Always = a
                      combineTimes t@(Once a) (Once b) = if a == b then t else Never
                      combineTimes t@(Once a) (Twice b c) = if a == b || a == c then t else Never
                      combineTimes (Twice a b) t@(Once c) = if a == c || b == c then t else Never
                      combineTimes t@(Twice a b) (Twice c d) = if | a == c && b == d -> t
                                                                  | a == c || a == d -> Once a
                                                                  | b == c || b == d -> Once b
                                                                  | otherwise -> Never

pairs :: [a] -> [(a,a)]
pairs (a:as) = pairs' [a] as
  where pairs' bs (a:as) = map ((,) a) bs ++ pairs' (a:bs) as
        pairs' bs [] = []

indSet :: [(Int, Int)] -> Set.Set Int
indSet = foldr (\(i1, i2) s -> Set.insert i1 $ Set.insert i2 s) Set.empty

collReduceMap :: Map.Map (Int, Int) CollisionTime -> Map.Map (Int, Int) CollisionTime
collReduceMap m =
  let accumMin vmin _ Never = (vmin, Never)
      accumMin vmin@(ixsmin, tmin) ix t = (
        case compare t tmin of
          LT -> ([ix], t)
          EQ -> (ix:ixsmin, tmin)
          GT -> vmin
          , t)
      colliding = indSet . fst . fst $ Map.mapAccumWithKey accumMin ([], Never) m
  in if Set.null colliding then m
     else collReduceMap $ Map.filterWithKey (\(i1, i2) _ -> not $ Set.member i1 colliding || Set.member i2 colliding ) m

removeCollisions :: [(Int, Particle)] -> [(Int, Particle)]
removeCollisions ps =
  let collTimePairs = map (\((i1, p1), (i2, p2)) -> ((i1, i2), collisionTime p1 p2)) $ pairs ps
      collmap = Map.fromList ([ ((i,i), Never) | (i,p) <- ps ] ++ collTimePairs )
      pSet = indSet $ Map.keys $ collReduceMap collmap
  in filter (\(i, _) -> i `Set.member` pSet) ps

minDist000  :: [(Int, Particle)] -> Int
minDist000 = fst . head . findMinima minR . findMinima minV . findMinima minA
  where findMinima f = fst . foldr minAppend ([], 1000000)
          where minAppend ip@(i, p) m@(mins, vmin) =
                  let vp = f p in
                  case compare vp vmin of
                    LT -> ([ip], vp)
                    EQ -> (ip:mins, vmin)
                    GT -> m
        minA (Particle _ _ (Vec3 x y z)) = abs x + abs y + abs z
        minV (Particle _ (Vec3 x y z) _) = abs x + abs y + abs z
        minR (Particle (Vec3 x y z) _ _) = abs x + abs y + abs z

main :: IO ()
main = solution <$> readFile "data/advent_20.txt" >>= putStrLn
  where solution file =
          let particles = zip [0..] . rights . map (parse particle "") $ lines file
          in "Part I : Particle #" ++ show (minDist000 particles) ++ " is closest to (0,0,0)\n" ++
             "Part II : " ++ show (length $ removeCollisions particles) ++ " particles left"
