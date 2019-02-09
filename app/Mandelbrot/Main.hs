{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Map
import Sector
-- import RepaMap
import Map.PixelMap
-- import SectorTree
-- import SectorMap
import Map.Dimension

import Linear.V as L
import Data.Complex
import qualified Data.Vector as V
import Codec.Picture
-- import Data.Kind

-- mandelbrot :: Int -> Complex Double -> Bool
{-# INLINE mandelbrot #-}
mandelbrot :: (Ord t, Num t, RealFloat a) => t -> Complex a -> Bool
mandelbrot n x = mandelbrot' x x n where
  mandelbrot' z _ _ | (sqr . magnitude $ z) > 4 = False
  mandelbrot' _ _ i | i <= 0 = True
  mandelbrot' z c i = mandelbrot' (z*z + c) c (i - 1)
  sqr a = a * a

{-# INLINE toComplexCoords #-}
toComplexCoords :: V 2 Double -> Complex Double
toComplexCoords (V v) = v V.! 1 :+ v V.! 0

{-# INLINE mandelMap #-}
mandelMap :: Int -> PixelMap Double
mandelMap n = fmap toGrayScale
            . changeCoordinates toComplexCoords
            $ getPoint >>= return . mandelbrot n

{-# INLINE mandelMap' #-}
mandelMap' :: Int -> Sector 2 Double -> Int -> PixelMap Double
mandelMap' n = optimizeMap (mandelMap n)

viewSector :: Zone -> Double -> Sector 2 Double
viewSector (Zone x y r) ratio = V . V.fromList $ [(x - r, x + r), (y - r * ratio, y + r * ratio)]

data Zone = Zone { zoneX :: Double
                 , zoneY :: Double
                 , zoneR :: Double }

interestingZones :: [Zone]
interestingZones =
  [ Zone 0 0 2
  , Zone 0.1 (-0.7) (5e-4)
  , Zone (0.109)  (-0.7443) (0.005)
  -- , Zone (0.1102) (-0.7463) (0.005)
  , Zone (0.1127) (-0.7453) (6.5e-4)
  , Zone (1.0405) (-0.16)   (0.026)
  ]

main :: IO ()
main = do
  let resX          = 1920 * resMultiplier -- x and y dimensions
      resY          = 1080 * resMultiplier
      resMultiplier = 1 -- increment this to get 4k, 8k, etc.
      ratio         = (fromIntegral resX) / (fromIntegral resY)
      zone          = Zone 0 0 2
      depth         = 400
      sec           = viewSector zone ratio
      img           = bakePixelMap
                        sec
                        (resolution resX resY)
                        (mandelMap depth)
  putStrLn "Execution started."
  savePngImage ("/tmp/mandelbrot.png") img
  putStrLn "Execution finished"
