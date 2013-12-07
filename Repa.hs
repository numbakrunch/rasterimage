module Repa where

import Control.Applicative ((<$>))
import Control.Arrow
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Applicative
import Data.Array.Repa hiding (map,(++))
import Data.Array.Repa.IO.BMP
import Data.Function (on)
import Data.Vect.Double
import Data.Vect.Double.Util.Quaternion
import Data.Vector.Unboxed.Base
import Data.Word

import Data.Colour.RGBSpace.HSV
import Data.Colour.RGBSpace

type Res = Int
type Pixel = Int

type ViewPlane = Pos
type Eye = Pos

compute :: (Unbox a) => Array D DIM2 a -> Array Data.Array.Repa.U DIM2 a
compute = runIdentity . computeUnboxedP

type LightSource = Vec3

{-# INLINE shadows #-}
shadows :: StepSize -> Hitter -> LightSource -> Maybe Vec3 -> (Word8, Word8, Word8)
shadows _ _ _ Nothing = (0,0,0)
shadows s h light (Just hit) = maybe (255,255,255) (const (0,0,0)) (runReader (generateVector s light >>= explore env) hit)
  where env = environment h 4 hit
          
{-# INLINE rainbowShadows #-}
rainbowShadows :: StepSize -> Hitter -> LightSource -> Maybe Vec3 -> (Word8, Word8, Word8)
rainbowShadows _ _ _ Nothing = (0,0,0)
rainbowShadows s h light (Just hit) = color $ runReader (generateVector s light >>= explore env) hit
  where color = maybe (uncurryRGB f (hsv rainbow 1 1)) (const (uncurryRGB f (hsv rainbow 1 0.2)))
        f x y z = (round (x * 255), round (y * 255), round (z * 255))
        rainbow = (2 - len hit) / 2 * 360
        env = environment h 4 hit
        
type StepSize = Double

{-# INLINE generateVector #-}
generateVector :: StepSize -> Eye -> Reader Pos (Pos -> Pos)
generateVector s vp = (&+) . scalarMul s . normalize . (&- vp) <$> ask

repaF :: DIM2 -> (Pixel,Pixel)
repaF (Z :. x :. y) = (,) x y

shape :: Int -> Int -> DIM2
shape = (:.) . (Z :.)

perspective :: Eye -> Double -> ViewPlane
perspective vp dist = vp &+ dist *& n
    where n = normalize . neg $ vp

{-# INLINE pixelCoord #-}
pixelCoord :: (Scale,Scale) -> (Res,Res) -> (Pixel,Pixel) -> Vec3
pixelCoord (sx,sy) (rx,ry) (px,py) = Vec3 x y z 
    where x = sx * pixelCoord' rx px
          y = sy * pixelCoord' ry py
          z = 0

{-# INLINE pixelCoord' #-}
pixelCoord' :: Res -> Pixel -> Double
pixelCoord' num p = (2 * p) // num - 1
    where (//) = (/) `on` fromIntegral
          
type Scale = Double
type Rotation = Mat3
type Offset = Vec3

orient :: Vec3 -> Reader Vec3 Vec3 
orient v = (&+ v) . (.* rotmat) <$> ask
    where rotmat = planeTranslation zNormal vNormal
          zNormal = mkNormal $ Vec3 0 0 1
          vNormal = mkNormal v

planeTranslation :: Normal3 -> Normal3 -> Rotation
planeTranslation n orient
    | theta == 0.0 = idmtx
    | isNaN (len axis) = idmtx .*. diag (Vec3 1 (-1) (-1))
    | otherwise = rotMatrix3' axis theta
    where axis = n &^ orient
          theta = angle' n orient

type Pos = Vec3
type Hitter = (Pos -> Bool)
type Environment = ReaderT Pos (Either (Maybe Pos)) Pos

{-# INLINE explore #-}
explore :: Environment -> (Pos -> Pos) -> Reader Pos (Maybe Pos)
explore r f = reader (go . f)
    where go = either id (go . f) . runReaderT r
               
environment :: Hitter -> Double -> Pos -> Environment
environment h dist p0 = checkDistance dist p0 *> checkHit h

checkDistance :: Double -> Pos -> Environment
checkDistance dist p0 = ask >>= lift . f
    where f pn | distance p0 pn < dist = Right pn
               | otherwise = Left Nothing

checkHit :: Hitter -> Environment
checkHit h = ask >>= lift . f
    where f p | h p = Left . Just $ p
              | otherwise = Right p
