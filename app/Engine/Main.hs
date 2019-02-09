{-# LANGUAGE DataKinds #-}
module Main where

-- import Linear.V
-- import Linear.V2
import Linear.V3 (V3 (..))
import Linear.Metric (normalize)

-- import Sector

import Map.Dimension (Resolution, resolution)

import Codec.Picture (DynamicImage, savePngImage)

import Engine

main :: IO ()
main = savePngImage ("/tmp/ImageTest.png") image

res :: Resolution 2
res = resolution (1920) (1080)

origin :: Vec3
origin = pure 0

cam :: Camera
cam = Camera 90 (V3 0 0 5) (normalize $ V3 1 0 (-1)) (V3 0 0 1) 1 res

scene :: [NormalObject]
scene = [ NormalObject $ Sphere 0.5 (V3 4 (-1) 0)
        , NormalObject $ Sphere 0.1 (V3 3.8 1 0.1)
        , NormalObject $ Sphere 0.1 (V3 4 0 0)
        , NormalObject $ Plane (V3 5 0 0) (V3 (-1) 0 0)
        , NormalObject $ Plane (V3 0 0 (-0.5)) (V3 0 0 1)
        , NormalObject $ Plane (V3 0 (-2) 0) (V3 0 1 0)
        , NormalObject $ Plane (V3 0 2 0) (V3 0 (-1) 0)
        ]

lights :: [PointLight]
lights = [ V3 0 0 4
         , V3 3.925 (-0.15) 0
         , V3 4 0 0
         ]

image :: DynamicImage
image = bakeScene cam scene lights
