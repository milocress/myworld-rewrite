{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

-- import Linear.V (fromV)
-- import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Linear.Metric (normalize)

-- import Map (getPoint)
import Map.SectorMap (DimensionalMap)
-- import Sector

import Map.Dimension (Resolution, resolution)

-- import Codec.Picture (DynamicImage, savePngImage)

import Engine

main :: IO ()
main = renderScene cam scene lights "/tmp/ImageTest.png"

type Precision = Double

res :: Resolution 2
res = resolution (1920) (1080)

origin :: V3 Precision
origin = pure 0

cam :: Camera Precision
cam = Camera 90 (V3 4 0 5) (normalize $ V3 0 0 (-1)) (normalize $ V3 (-1) 0 0) 1 res

scene :: [NormalObject Precision]
scene = [
        --   NormalObject $ (Sphere 0.5 (V3 4 (-1) 0)         :: Sphere Precision)
        -- , NormalObject $ (Sphere 0.1 (V3 3.8 1 0.1)        :: Sphere Precision)
        -- , NormalObject $ (Sphere 0.1 (V3 4 0 0)            :: Sphere Precision)
        -- , NormalObject $ (Plane (V3 5 0 0) (V3 (-1) 0 0)   :: Plane  Precision)
        -- , NormalObject $ (Plane (V3 0 0 (-0.5)) (V3 0 0 1) :: Plane  Precision)
        -- , NormalObject $ (Plane (V3 0 (-2) 0) (V3 0 1 0)   :: Plane  Precision)
        -- , NormalObject $ (Plane (V3 0 2 0) (V3 0 (-1) 0)   :: Plane  Precision)
        NormalObject $ (return 0 :: DimensionalMap 2 Precision Precision)
        ]

lights :: [PointLight Precision]
lights = [ V3 0 0 4
         -- , V3 3.925 (-0.15) 0
         -- , V3 4 0 0
         ]
