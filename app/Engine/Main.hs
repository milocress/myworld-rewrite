{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Linear.Metric (normalize)

import Numeric.AD (grad)

import Map (getPoint)

import Map.Dimension (Resolution, resolution)

import Engine

main :: IO ()
main = renderScene myEngine cam scene lights "/tmp/ImageTest.png"

type FloatPrecision = Double
type IntPrecision   = Int

myEngine :: EngineConfig FloatPrecision IntPrecision
myEngine = EngineConfig { maxSteps        = 100
                        , maxDist         = 1000
                        , minDist         = 1e-6
                        , shadowsEnabled  = True
                        }

res :: Resolution 2
res = 10 * resolution (192) (108)

origin :: V3 FloatPrecision
origin = pure 0

cam :: Camera FloatPrecision
cam = Camera 150
             (V3 (-1) 0 4)                -- position
             (normalize $ V3 1 0 (-1)) -- lookAt
             (normalize $ V3 1 0 1)    -- camUp
             1                         -- scale
             res                       -- resolution

scene :: [NormalObject FloatPrecision]
scene = [
          NormalObject $ ( Sphere 0.5 (V3 4 (-1) 0)         :: Sphere FloatPrecision )
        , NormalObject $ ( Sphere 0.1 (V3 3.8 1 0.1)        :: Sphere FloatPrecision )
        , NormalObject $ ( Sphere 0.1 (V3 4 0 0)            :: Sphere FloatPrecision )
        , NormalObject $ ( Plane (V3 5 0 0) (V3 (-1) 0 0)   :: Plane  FloatPrecision )
        , NormalObject $ ( Plane (V3 0 0 (-0.5)) (V3 0 0 1) :: Plane  FloatPrecision )
        , NormalObject $ ( Plane (V3 0 (-2) 0) (V3 0 1 0)   :: Plane  FloatPrecision )
        , NormalObject $ ( Plane (V3 0 2 0) (V3 0 (-1) 0)   :: Plane  FloatPrecision )
        -- , NormalObject groundMap
        --   NormalObject groundMap
        -- , NormalObject $ ( Plane (V3 0 (-2) 0) (V3 0 1 0)   :: Plane  FloatPrecision )
        -- , NormalObject $ ( Plane (V3 0 2 0) (V3 0 (-1) 0)   :: Plane  FloatPrecision )
        ]

groundMap :: GradMap2 FloatPrecision
groundMap = do
  p <- getPoint
  return $ GradMapInfo (f p) (grad f p)

f :: (Floating a) => V2 a -> a
f (V2 x y) = 0.1 * (sin (x * 3)) * (sin (y * 3)) - 0.5

lights :: [PointLight FloatPrecision]
lights = [ V3 0 0 3
         -- , V3 4 0 4
         -- , V3 3.925 (-0.15) 1
         -- , V3 4 0 1
         ]
