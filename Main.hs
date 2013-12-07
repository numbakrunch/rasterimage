module Main where

import Data.Word

import Data.Vect.Double.Util.Quaternion
import Data.Vect.Double.Base

import Control.Monad.Identity
import Data.Array.Repa hiding (map,(++))
import Data.Array.Repa.IO.BMP
import GHC.Conc

import Repa

import Control.Monad.Reader

import Data.Maybe

import Fractal

c = Q $ Vec4 (-0.2) 0.6 0.2 0.2

c'' = Q $ Vec4 (-0.218) (-0.113) (-0.181) (-0.496)

main = let
     xs = [pi * 7/8]  
--     xs = [0,pi/8..2*pi]
  in do
  print numCapabilities
  mapM_ (\(y,x) -> writeImageToBMP ("Test" ++ show y ++ ".bmp") . compute $ stitchRepa (200,200) 0.01 (Vec3 0 (10 * sin x) (10 * cos x)) (9)) $ zip [0..] xs
            
{-# NOINLINE stitchRepa #-}
stitchRepa :: (Res,Res) -> StepSize -> Eye -> Double -> Array D DIM2 (Word8,Word8,Word8)
stitchRepa r step eye d = fromFunction (uncurry shape r) $ shading . (z . o) . pixelCoord (1.5,1.5) r . repaF
  where vp = perspective eye d
        o = runReader (orient vp)
        z = runReader (generateVector step eye >>= explore (environment hitter 4 vp))
        hitter = isJust . evalFractal (iterateFractal_ 50 $ juliaFractal c') . toQ . sliceY 0.0
        shading = rainbowShadows step hitter (Vec3 0 10 10)
--        shading = shadows step hitter (Vec3 0 10 10)
                                                       
