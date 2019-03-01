{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Linear.Metric (normalize, distance)

import Numeric.AD (grad, gradientDescent)

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
                        , shadowsEnabled  = False
                        }

res :: Resolution 2
res = 2 * resolution (192) (108)

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

groundMap :: GradMap2 FloatPrecision
groundMap = do
  p <- getPoint
  let f (V2 x y) = 0.4 * (sin (x * 6)) * (sin y) - 0.5
      g = grad f p
      -- Minimum Distance
      d [ c1, c2, c3
        , d1, d2] = distance (V3 c1 c2 c3) (V3 d1 d2 $ f (V2 d1 d2))
      d _ = error "Internal gradient descent error"
      nearestPoint (V3 x y z) = toV3'' $ head $ gradientDescent d [x, y, z, x, y]
      -- toV3 xs = V3 x y $ f (V2 x y) where [_, _, _, x, y] = xs
      -- toV3' xs = V3 x y z where [x, y, z, _, _] = xs
      toV3'' xs = V3 x y z where [_, _, z, x, y] = xs
  return $ GradMapInfo (f p) g nearestPoint
         -- $ (\point ->
         --      let point' = nearestPoint point
         --      in point' + 2 * (point - point'))

lights :: [PointLight FloatPrecision]
lights = [ V3 0 0 3
         -- , V3 4 0 4
         -- , V3 3.925 (-0.15) 1
         -- , V3 4 0 1
         ]
