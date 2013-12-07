import Data.Vect.Double

data Hit a = None | Intersection a a
           deriving Show

type Radius = Double
type Sphere = (Vec3,Radius)

explores :: Vec3 -> Vec3 -> Sphere -> Maybe Vec3
explores d p0 (pc,r) = 
  case sphereIntersection d p0 (pc,r) of
    None -> Nothing
    Intersection i o -> Just $ p0 &+ (i *& d)    

{- from
  http://wiki.cgsociety.org/index.php/Ray_Sphere_Intersection
-}

sphereIntersection :: Vec3 -> Vec3 -> Sphere -> Hit Double
sphereIntersection d p0 (pc,r)
    | q' >= 0 = Intersection t0 t1
    | otherwise = None
    where t0 = c / q
          t1 = q / a
          q | b < 0 = (negate b + q'') / 2
            | otherwise = (negate b - q'') / 2
          q'' = sqrt q'
          q' = b^2 - 4 * a * c
          a = d &. d
          b = (2 *& d) &. dist
          c = dist &. dist - r^2
          dist = (p0 &- pc)
