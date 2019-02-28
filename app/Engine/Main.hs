{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..), cross)
import Linear.Metric (normalize, dot)

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
                        , minDist         = 1e-4
                        , shadowsEnabled  = True
                        }

res :: Resolution 2
res = resolution (1920) (1080)

origin :: V3 FloatPrecision
origin = pure 0

cam :: Camera FloatPrecision
cam = Camera 90                        -- field of view
             (V3 0 0 4)                -- position
             (normalize $ V3 1 0 (-1)) -- lookAt
             (normalize $ V3 1 0 1)    -- camUp
             1                         -- scale
             res                       -- resolution

scene :: [NormalObject FloatPrecision]
scene = [
        --   NormalObject $ ( Sphere 0.5 (V3 4 (-1) 0)         :: Sphere FloatPrecision )
        -- , NormalObject $ ( Sphere 0.1 (V3 3.8 1 0.1)        :: Sphere FloatPrecision )
        -- , NormalObject $ ( Sphere 0.1 (V3 4 0 0)            :: Sphere FloatPrecision )
        -- , NormalObject $ ( Plane (V3 5 0 0) (V3 (-1) 0 0)   :: Plane  FloatPrecision )
        -- , NormalObject $ ( Plane (V3 0 0 (-0.5)) (V3 0 0 1) :: Plane  FloatPrecision )
        -- , NormalObject $ ( Plane (V3 0 (-2) 0) (V3 0 1 0)   :: Plane  FloatPrecision )
        -- , NormalObject $ ( Plane (V3 0 2 0) (V3 0 (-1) 0)   :: Plane  FloatPrecision )
        -- , NormalObject groundMap
          NormalObject groundMap
        , NormalObject $ ( Plane (V3 0 (-2) 0) (V3 0 1 0)   :: Plane  FloatPrecision )
        , NormalObject $ ( Plane (V3 0 2 0) (V3 0 (-1) 0)   :: Plane  FloatPrecision )
        ]

groundMap :: DualMap2 FloatPrecision
groundMap = do
  p <- getPoint
  let f (V2 x y) = 0.1 * (sin (x * 3)) * (sin y) - 0.5
      g  = grad f p
      dz = g `dot` normalize g
      z  = V3 0 0 1
      g3 = promote g
      promote (V2 a b) = V3 a b 0
      normal = normalize $ cross
        (normalize g3 + pure dz * z)
        (cross z g3)
  return (f p, normal)

lights :: [PointLight FloatPrecision]
lights = [ V3 0 0 3
         -- , V3 4 0 4
         -- , V3 3.925 (-0.15) 1
         -- , V3 4 0 1
         ]
