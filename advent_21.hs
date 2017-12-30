import Text.Megaparsec
import Text.Megaparsec.Error
import Text.Megaparsec.Char
import Data.Void (Void)

import Data.List (intercalate)

import Debug.Trace

import qualified Data.Map as Map

import qualified Data.Array.Unboxed as UA
import qualified Data.Array.IArray as IA

type Slice2x2 = (Bool, Bool, Bool, Bool)
type Slice3x3 = (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)
type Slice4x4 = (Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool)

type Image = UA.UArray (Int, Int) Bool

type Map2x2 = Map.Map Slice2x2 Slice3x3
type Map3x3 = Map.Map Slice3x3 Slice4x4

type Parser = Parsec Void String

map2x2 :: (Slice2x2 -> Slice3x3) -> Image -> Image
map2x2 f im =
  let ((i0,j0), (iN, jN)) = IA.bounds im
      _3x3s = [ (3 * div (x-i0) 2,
                 3 * div (y-j0) 2,
                 f (im IA.! (x,y),
                    im IA.! (x+1,y),
                    im IA.! (x,y+1),
                    im IA.! (x+1,y+1)
                    )
                 )
                 | x <- [i0,i0+2..iN], y <- [j0,j0+2..jN] ]
  in IA.array ((0,0), (3 * div (iN - i0 + 1) 2 - 1, 3 * div (jN - j0 + 1) 2 - 1)) $ concatMap expand _3x3s
  where expand (ii, ij, (a,b,c,d,e,f,g,h,i)) =
          [((ii, ij), a),
           ((ii+1, ij), b),
           ((ii+2, ij), c),
           ((ii, ij+1), d),
           ((ii+1, ij+1), e),
           ((ii+2, ij+1), f),
           ((ii, ij+2), g),
           ((ii+1, ij+2), h),
           ((ii+2, ij+2), i)]

map3x3 :: (Slice3x3 -> Slice4x4) -> Image -> Image
map3x3 f im =
  let ((i0,j0), (iN, jN)) = IA.bounds im
      _4x4s = [ (4 * div (x-i0) 3,
                 4 * div (y-j0) 3,
                 f (im IA.! (x,y),
                    im IA.! (x+1,y),
                    im IA.! (x+2,y),
                    im IA.! (x,y+1),
                    im IA.! (x+1,y+1),
                    im IA.! (x+2,y+1),
                    im IA.! (x,y+2),
                    im IA.! (x+1,y+2),
                    im IA.! (x+2,y+2)
                    )
                 )
                 | x <- [i0,i0+3..iN], y <- [j0,j0+3..jN] ]
  in IA.array ((0,0), (4 * div (iN - i0 + 1) 3 - 1, 4 * div (jN - j0 + 1) 3 - 1)) $ concatMap expand _4x4s
  where expand (ii, ij, (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p)) =
          [((ii, ij), a),
           ((ii+1, ij), b),
           ((ii+2, ij), c),
           ((ii+3, ij), d),
           ((ii, ij+1), e),
           ((ii+1, ij+1), f),
           ((ii+2, ij+1), g),
           ((ii+3, ij+1), h),
           ((ii, ij+2), i),
           ((ii+1, ij+2), j),
           ((ii+2, ij+2), k),
           ((ii+3, ij+2), l),
           ((ii, ij+3), m),
           ((ii+1, ij+3), n),
           ((ii+2, ij+3), o),
           ((ii+3, ij+3), p)]

orientations2x2 :: (a,a,a,a) -> [(a,a,a,a)]
orientations2x2 (a,b,c,d) = [
  (a,b,c,d), (b,a,d,c),
  (a,c,b,d), (c,a,d,b),
  (c,d,a,b), (d,c,b,a),
  (b,d,a,c), (d,b,c,a) ]

orientations3x3 :: (a,a,a,a,a,a,a,a,a) -> [(a,a,a,a,a,a,a,a,a)]
orientations3x3 (a,b,c,d,e,f,g,h,i) = [
  (a,b,c,d,e,f,g,h,i), (c,b,a,f,e,d,i,h,g),
  (a,d,g,b,e,h,c,f,i), (g,d,a,h,e,b,i,f,c),
  (c,f,i,b,e,h,a,d,g), (i,f,c,h,e,b,g,d,a),
  (g,h,i,d,e,f,a,b,c), (i,h,g,f,e,d,c,b,a) ]


pixel :: Parser Bool
pixel = (char '#' >> return True) <|> (char '.' >> return False)

showImage :: Image -> String
showImage im =
  let ((i0,j0), (iN, jN)) = IA.bounds im
  in intercalate "\n" [ [p $ im IA.! (x,y) | x <- [i0..iN]] | y <- [j0..jN] ]
  where p b = if b then '#' else '.'


imageRule2x2 :: Parser (Slice2x2, Slice3x3)
imageRule2x2 =
  do  [sa,sb] <- count 2 pixel
      _ <- char '/'
      [sc,sd] <- count 2 pixel
      string " => "
      [ea,eb,ec] <- count 3 pixel
      _ <- char '/'
      [ed,ee,ef] <- count 3 pixel
      _ <- char '/'
      [eg,eh,ei] <- count 3 pixel
      space
      return ((sa,sb,sc,sd), (ea,eb,ec,ed,ee,ef,eg,eh,ei))

imageRule3x3 :: Parser (Slice3x3, Slice4x4)
imageRule3x3 =
  do  [sa,sb,sc] <- count 3 pixel
      _ <- char '/'
      [sd,se,sf] <- count 3 pixel
      _ <- char '/'
      [sg,sh,si] <- count 3 pixel
      string " => "
      [ea,eb,ec,ed] <- count 4 pixel
      _ <- char '/'
      [ee,ef,eg,eh] <- count 4 pixel
      _ <- char '/'
      [ei,ej,ek,el] <- count 4 pixel
      _ <- char '/'
      [em,en,eo,ep] <- count 4 pixel
      return ((sa,sb,sc,sd,se,sf,sg,sh,si), (ea,eb,ec,ed,ee,ef,eg,eh,ei,ej,ek,el,em,en,eo,ep))

imageRules :: Parser (Map2x2, Map3x3)
imageRules =
  do r2x2 <- concatMap (\(a,b) -> map (flip (,) b) $ orientations2x2 a) <$> sepEndBy (try imageRule2x2) space
     r3x3 <- concatMap (\(a,b) -> map (flip (,) b) $ orientations3x3 a) <$> sepEndBy imageRule3x3 space
     eof
     return (Map.fromList r2x2, Map.fromList r3x3)

mapImage :: Map2x2 -> Map3x3 -> Int -> Image -> Image
mapImage _ _ 0 im = im
mapImage m2 m3 n im =
  let ((i0,_), (iN,_)) = IA.bounds im
      imIt = if mod (iN-i0+1) 2 == 0 then map2x2 (m2 Map.!) im else map3x3 (m3 Map.!) im
  in mapImage m2 m3 (n-1) imIt

countSet :: Image -> Int
countSet = foldr (\a b -> b + fromEnum a) 0 . IA.elems

main :: IO ()
main = solution <$> readFile "data/advent_21.txt" >>= print
  where solution file =
          case parse imageRules "" file of
            Left e -> error $ parseErrorPretty e
            Right (m2, m3) -> countSet $ mapImage m2 m3 18 imageZero
        imageZero = IA.listArray ((1,1), (3,3)) [False, True, False, False, False, True, True, True, True]
