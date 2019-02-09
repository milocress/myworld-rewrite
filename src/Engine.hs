{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
module Engine ( Object (..)
              , NormalObject (..)
              , Vec3
              , Map3
              , Camera (..)
              , bakeScene
              , PointLight
              , Sphere (..)
              , Plane (..)
              ) where

import Map
import Map.SectorMap
import Map.Dimension
import Map.PixelMap
-- import Map.RepaMap

import Linear.V2
import Linear.V3
import Linear.V
import Linear.Metric (norm, normalize, dot, distance)
-- import Data.Word
import Data.List (genericLength, minimumBy)
import Data.Function (on)
import Data.Maybe (fromMaybe)

import Control.Monad (guard)
import Codec.Picture (DynamicImage)

type Vec3  = V3 Double

-- type Color = (Word8, Word8, Word8)

type Map3 a = DimensionalMap 3 Double a

minDist, maxDist :: Double
minDist = 1e-4
maxDist = 1e2

maxSteps :: Int
maxSteps = 1000

class ObjectC a where
  {-# MINIMAL (sdf | sdfMap) #-}
  sdf :: Vec3 -> a -> Double
  sdf p obj = runMap (sdfMap obj) (toV p)

  adf :: Vec3 -> a -> Double
  adf p = abs . sdf p

  sdfMap :: a -> Map3 Double
  sdfMap obj = do
    p <- fromV <$> getPoint
    return $ sdf p obj

class ObjectC a => NormalC a where
  normal :: Vec3 -> a -> Vec3
  normal p scene = normalize $ V3
    (de (p + x * epsilon) - de (p - x * epsilon))
    (de (p + y * epsilon) - de (p - y * epsilon))
    (de (p + z * epsilon) - de (p - z * epsilon))
    where
      de = flip sdf scene
      x = V3 1 0 0
      y = V3 0 1 0
      z = V3 0 0 1
      epsilon = pure minDist

instance ObjectC Vec3 where
  sdf p v = norm $ v - p

instance ObjectC a => ObjectC [a] where
  sdf p = minimum . fmap (sdf p)

instance NormalC a => NormalC [a] where
  normal p = normal p . minimumBy (compare `on` sdf p)

instance (ObjectC a, ObjectC b) => ObjectC (a, b) where
  sdf p (a, b) = min (sdf p a) (sdf p b)

traceDist :: ObjectC a => Vec3 -> Vec3 -> a -> Maybe Double
traceDist point ray scene = go 0 (0 :: Int) where
  go d n = do
    let dist = sdf (point + pure d * ray) scene
    guard $ n < maxSteps
    guard $ dist < maxDist
    if dist < minDist
      then return d
      else go (dist + d) (succ n)

traceRay :: ObjectC a => Vec3 -> Vec3 -> a -> Maybe Vec3
traceRay point ray scene = do
  d <- traceDist point ray scene
  return $ point + ray * pure d

traceColor :: NormalC a => Vec3 -> Vec3 -> a -> [PointLight] -> Maybe RGB8
traceColor point ray scene lights = do
  p <- traceRay    point ray scene
  let n = normal p scene
      ambient = 0.1
      diffuse = average $ do
                  l <- lights
                  let shade = shadow p n scene l
                  return . (* shade)
                         . clamp 0 1
                         $ dot (normalize $ l - p) n
      c = fromIntegral <$> white
  return $ floor <$> clamp 0 255 <$> c * pure ambient + c * pure diffuse

average :: Fractional a => [a] -> a
average xs = sum xs / genericLength xs

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

shadow :: ObjectC a => Vec3 -> Vec3 -> a -> PointLight -> Double
shadow point n scene l =
  let point' = point + n * pure minDist
  in fromMaybe 0.0 $ do
    p <- traceRay point' (normalize $ l - point') (scene, l)
    return $ if p `near` l then 1.0 else 0.0

near :: Vec3 -> Vec3 -> Bool
near v w = (abs $ distance v w) <= minDist

data Camera = Camera { camFov :: Double       -- ^ angle, in degrees
                     , camPos :: Vec3         -- ^ x, y, and z-coordinates
                     , camFacing :: Vec3         -- ^ Normal vector of the camera's face (the direction the camera is facing)
                     , camUp  :: Vec3         -- ^ Must be perpendicular to camFacing
                     , camScale :: Double       -- ^ scale
                     , camRes :: Resolution 2 -- ^ resolution
                     }

bakeScene :: NormalC a => Camera -> a -> [PointLight] -> DynamicImage
bakeScene cam@Camera{..} objects lights = bakePixelMap sec camRes m where
  sec = toSector camRes
  m = do
    p <- uvToWorld cam <$> fromV <$> getPoint
    return . fromMaybe black
           . traceColor p (normalize $ p - camPos) objects
           $ lights

ratio :: Resolution 2 -> Double
ratio res = let
  (V2 x y) = fromV $ fromIntegral <$> res
  in y / x

uvToWorld :: Camera -> V2 Double -> V3 Double
uvToWorld cam (V2 u v) = camPos
                       + (camFacing * pure camScale)
                       + (pure lenX * dir)
                       + (pure lenY * camUp) where
  (Camera{..}) = hackCam cam
  dir = normalize $ cross camUp camFacing
  (V2 resX resY) = fromV $ fromIntegral <$> camRes
  lenX = ((u / resX) - 0.5) * scale
  lenY = ((v / resY) - 0.5) * scale * ratio camRes
  scale = (sin . toRadians $ (camFov / 2)) * camScale

wierdHack :: Vec3 -> Vec3
wierdHack (V3 x z y) = V3 x y z

hackCam :: Camera -> Camera
hackCam c@Camera{..} = c { camUp = wierdHack camUp }

toRadians :: Double -> Double
toRadians theta = pi * theta / 180

data Sphere = Sphere { sphereRadius :: Double
                     , spherePos    :: Vec3
                     }

instance ObjectC Sphere where
  sdf p Sphere{..} = norm (p - spherePos) - sphereRadius

instance NormalC Sphere where
  normal p Sphere{..} = normalize $ p - spherePos

type PointLight = Vec3

data Plane = Plane { planePoint  :: Vec3
                   , planeNormal :: Vec3
                   }

instance ObjectC Plane where
  sdf p Plane{..} = dot (p - planePoint) planeNormal

instance NormalC Plane where
  normal _ Plane{..} = planeNormal

data Object = forall a . ObjectC a => Object a
data NormalObject = forall a . NormalC a => NormalObject a

instance ObjectC Object where
  sdf p (Object o) = sdf p o
  adf p (Object o) = adf p o

instance ObjectC NormalObject where
  sdf p (NormalObject o) = sdf p o
  adf p (NormalObject o) = adf p o

instance NormalC NormalObject where
  normal p (NormalObject o) = normal p o

