{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Linear.V (fromV)
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Linear.Metric (normalize)

import Map (getPoint)
-- import Map.SectorMap (DimensionalMap)
-- import Sector

import Map.Dimension (Resolution, resolution)

-- import Codec.Picture (DynamicImage, savePngImage)

import Engine

main :: IO ()
main = renderScene myEngine cam scene lights "/tmp/ImageTest.png"

type FloatPrecision = Double
type IntPrecision   = Int

myEngine :: EngineConfig FloatPrecision IntPrecision
myEngine = EngineConfig { maxSteps = 100
                        , maxDist  = 1000
                        , minDist  = 1e-4
                        }

res :: Resolution 2
res = resolution (1920) (1080)

origin :: V3 FloatPrecision
origin = pure 0

cam :: Camera FloatPrecision
cam = Camera 90                        -- field of view
             (V3 4 0 4)                -- position
             (normalize $ V3 0 0 (-1))    -- lookAt
             (normalize $ V3 1 0 0)    -- camUp
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
        -- , NormalObject $ ( (do
        --                        (V2 x y) <- fromV <$> getPoint
        --                        let bump = sin (x * roughness) + sin (y * roughness)
        --                            roughness = 10
        --                        return $ 0.01 * bump - 0.5
        --                    ) :: Map2   FloatPrecision )
        ]

lights :: [PointLight FloatPrecision]
lights = [ V3 0 0 4
         , V3 3.925 (-0.15) 0
         , V3 4 0 0
         ]
