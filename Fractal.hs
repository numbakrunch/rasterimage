{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fractal where

import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Identity
import Control.Monad.Instances

import Data.Vect.Double.Base
import Data.Vect.Double.Util.Quaternion

type Fractal = FractalT Identity

fractal :: (Q -> Maybe (a,Q)) -> Fractal a
fractal f = fractalT (Identity . f)

runFractal :: Fractal a -> Q -> Maybe (a,Q)
runFractal f = runIdentity . runFractalT f

evalFractal :: Fractal a -> Q -> Maybe a
evalFractal f = fmap fst . runFractal f

execFractal :: Fractal a -> Q -> Maybe Q
execFractal f = fmap snd . runFractal f

iterateFractal :: Int -> Fractal a -> Fractal [a]
iterateFractal = iterateFractalT

iterateFractal_ :: Int -> Fractal a -> Fractal ()
iterateFractal_ = iterateFractalT_

newtype FractalT m a = FractalT (StateT Q (MaybeT m) a)
    deriving (Monad,Functor)

fractalT :: (Q -> m (Maybe (a,Q))) -> FractalT m a
fractalT f = FractalT (StateT (MaybeT . f))

runFractalT :: FractalT m a -> Q -> m (Maybe (a,Q))
runFractalT (FractalT f) = runMaybeT . runStateT f

evalFractalT :: (Monad m) => FractalT m a -> Q -> m (Maybe a)
evalFractalT f = liftM (liftM fst) . runFractalT f

execFractalT :: (Monad m) => FractalT m a -> Q -> m (Maybe Q)
execFractalT f = liftM (liftM snd) . runFractalT f

iterateFractalT :: (Monad m) => Int -> FractalT m a -> FractalT m [a]
iterateFractalT = replicateM

iterateFractalT_ :: (Monad m) => Int -> FractalT m a -> FractalT m ()
iterateFractalT_ = replicateM_

{- Julia set and power helper, should remove to stuff vector math other places -}

juliaFractal :: Q -> Fractal Q
juliaFractal c = fractal f
    where f q | not $ escape q = Just (q,julia q)
              | otherwise = Nothing
          julia x = (x `multQ` x) &+ c
          escape x = len x > 4

c = toQ $ Vec4 (-0.2) 0.6 0.2 0.2
c' = toQ $ Vec4 (-0.162) 0.163 0.560 (-0.599)

type Slicer = Vec3 -> Vec4

sliceX :: Double -> Slicer
sliceX d (Vec3 x y z) = Vec4 d x y z

sliceY :: Double -> Slicer
sliceY d (Vec3 x y z) = Vec4 x d y z

sliceZ :: Double -> Slicer
sliceZ d (Vec3 x y z) = Vec4 x y d z

sliceW :: Double -> Slicer
sliceW d (Vec3 x y z) = Vec4 x y z d
