{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Engine ( Object (..)
              , NormalObject (..)
              , Map2
              , Map3
              , Camera (..)
              , renderScene
              , PointLight
              , Sphere (..)
              , Plane (..)
              , EngineConfig (..)
              ) where

import Engine.Class
import Engine.Config
import Engine.Trace
import Engine.Light

import Map
import Map.Dimension

import Linear.V2
import Linear.V3
import Linear.V
import Linear.Metric (normalize)
import Linear.Epsilon (Epsilon)
import Linear.Quaternion (rotate, axisAngle)
import Linear.Conjugate (Conjugate)

import Data.Maybe (fromMaybe)


import Map.PixelMap (black, writePixelMap)

data Camera a = Camera { camFov    :: a            -- ^ angle, in degrees
                       , camPos    :: V3 a
                       , camFacing :: V3 a         -- ^ Normal vector of the camera's face (the direction the camera is facing)
                       , camUp     :: V3 a         -- ^ Must be perpendicular to camFacing
                       , camScale  :: a            -- ^ scale
                       , camRes    :: Resolution 2 -- ^ resolution
                       }

renderScene :: (RealFrac a, NormalC p a, Epsilon a, Floating a, Conjugate a, RealFloat a, Integral b)
            => EngineConfig a b -> Camera a -> p -> [V3 a] -> FilePath -> IO ()
renderScene conf cam@Camera{..} objects lights path = writePixelMap path camRes m where
  m = do
    p <- uvToWorld cam <$> fromV <$> getPoint
    return . fromMaybe black
           . traceColor conf p (normalize $ p - camPos) objects
           $ lights

ratio :: ( Num a
         , Fractional a
         )
      => Resolution 2 -> a
ratio res = let
  (V2 x y) = fromV $ fromIntegral <$> res
  in y / x

uvToWorld :: ( Num a
             , Floating a
             , Epsilon a
             , Conjugate a
             , RealFloat a
             )
          => Camera a -> V2 a -> V3 a
uvToWorld Camera{..} (V2 u v) = camPos
                              + (camFacing * pure camScale)
                              + (pure lenX * dir)
                              + (pure lenY * camUp) where
  dir = normalize $ rotate (axisAngle camFacing $ 3 * pi / 2) camUp
  (V2 resX resY) = fromV $ fromIntegral <$> camRes
  -- I have no idea why these work, but don't touch it!
  lenY = -((u / resX) - 0.5) * scale
  lenX = ((v / resY) - 0.5) * scale * ratio camRes

  scale = (sin . toRadians $ (camFov / 2)) * camScale

toRadians :: ( Fractional a
             , Floating a ) => a -> a
toRadians theta = pi * theta / 180

