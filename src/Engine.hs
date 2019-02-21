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
              ) where

import Map
import Map.SectorMap
import Map.Dimension

import Linear.V2
import Linear.V3
import Linear.V
import Linear.Metric (norm, dot, distance, normalize)
import Linear.Epsilon (Epsilon)

import Data.List (genericLength, minimumBy)
import Data.Function (on)
import Data.Maybe (fromMaybe)

import Control.Monad (guard)

-- import Map.AccMap (Color, white, black, writePixelMap)
import Map.PixelMap (white, black, writePixelMap)

-- import qualified Data.Array.Accelerate as A

-- type Map3 c a = DimensionalMap 3 c a

minDist, maxDist :: Fractional a => a
minDist = 1e-4
maxDist = 1e2

maxSteps :: Int
maxSteps = 1000

class ObjectC s a where
  sdf :: V3 a -> s -> a

class ObjectC s a => NormalC s a where
  normal :: ( Floating a
            , Epsilon a
            , Ord a
            )
         => V3 a -> s -> V3 a
  normal p scene = normalize $ V3
    (de (p + x) - de (p - x))
    (de (p + y) - de (p - y))
    (de (p + z) - de (p - z))
    where
      de = flip sdf scene
      x = V3 epsilon 0 0
      y = V3 0 epsilon 0
      z = V3 0 0 epsilon
      epsilon = minDist

instance Floating a => ObjectC (V3 a) a where
  sdf p v = norm $ v - p

instance ( ObjectC s a
         , Ord a
         ) => ObjectC [s] a where
  sdf p = minimum . fmap (sdf p)

instance ( NormalC s a
         , Ord a
         ) => NormalC [s] a where
  normal p = normal p . minimumBy (compare `on` sdf p)

instance ( ObjectC s a
         , ObjectC t a
         , Ord a ) => ObjectC (s, t) a where
  sdf p (a, b) = min (sdf p a) (sdf p b)

traceDist :: ( ObjectC s a
             , Ord a
             , Num a
             , Fractional a
             , Floating a
             ) => V3 a -> V3 a -> s -> Maybe a
traceDist point ray scene = go 0 (0 :: Int) where
  go d n = do
    let dist = sdf (point + pure d * ray) scene
    guard $ n < maxSteps
    guard $ dist < maxDist
    if dist < minDist
      then return d
      else go (dist + d) (succ n)

traceRay :: ( ObjectC s a
            , Ord a
            , Fractional a
            , Floating a
            )
         => V3 a -> V3 a -> s -> Maybe (V3 a)
traceRay point ray scene = do
  d <- traceDist point ray scene
  return $ point + ray * pure d

traceColor :: (Floating a, Epsilon a, NormalC s a, Integral b, RealFrac a)
           => V3 a -> V3 a -> s -> [PointLight a] -> Maybe (V3 b)
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
  return $ floor <$> clamp 0 255 <$> c * pure (ambient + diffuse)

average :: Fractional a => [a] -> a
average xs = sum xs / genericLength xs

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

shadow :: ( ObjectC s a
          , Num a
          , Fractional a
          , Ord a
          , Epsilon a
          , Floating a
          )
       => V3 a -> V3 a -> s -> PointLight a -> a
shadow point n scene l =
  let point' = point + n * pure minDist
  in fromMaybe 0.0 $ do
    p <- traceRay point' (normalize $ l - point') (scene, l)
    return $ if p `near` l then 1.0 else 0.0

near :: ( Num a
        , Floating a
        , Ord a
        )
     => V3 a -> V3 a -> Bool
near v w = (abs $ distance v w) <= minDist

data Camera a = Camera { camFov    :: a            -- ^ angle, in degrees
                       , camPos    :: V3 a
                       , camFacing :: V3 a         -- ^ Normal vector of the camera's face (the direction the camera is facing)
                       , camUp     :: V3 a         -- ^ Must be perpendicular to camFacing
                       , camScale  :: a            -- ^ scale
                       , camRes    :: Resolution 2 -- ^ resolution
                     }

renderScene :: (RealFrac a, NormalC p a, Epsilon a, Floating a)
            => Camera a -> p -> [V3 a] -> FilePath -> IO ()
renderScene cam@Camera{..} objects lights path = writePixelMap path camRes m where
  m = do
    p <- uvToWorld cam <$> fromV <$> getPoint
    return . fromMaybe black
           . traceColor p (normalize $ p - camPos) objects
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
             )
          => Camera a -> V2 a -> V3 a
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

wierdHack :: V3 a -> V3 a
wierdHack (V3 x y z) = V3 y x z

hackCam :: Camera a -> Camera a
hackCam c@Camera{..} = c { camUp = wierdHack camUp }

toRadians :: ( Fractional a
             , Floating a ) => a -> a
toRadians theta = pi * theta / 180

data Sphere a = Sphere { sphereRadius :: a
                       , spherePos    :: V3 a
                       }

instance ( Num a
         , Floating a
         ) => ObjectC (Sphere a) a where
  sdf p Sphere{..} = norm (p - spherePos) - sphereRadius

instance (Floating a) => NormalC (Sphere a) a where
  normal p Sphere{..} = normalize $ p - spherePos

type PointLight a = V3 a

data Plane a = Plane { planePoint  :: V3 a
                     , planeNormal :: V3 a
                     }

instance (Num a) => ObjectC (Plane a) a where
  sdf p Plane{..} = dot (p - planePoint) planeNormal

instance (Num a) => NormalC (Plane a) a where
  normal _ Plane{..} = planeNormal

data Object a = forall s . ObjectC s a => Object s
data NormalObject a = forall s . NormalC s a => NormalObject s

instance ObjectC (Object a) a where
  sdf p (Object o) = sdf p o

instance ObjectC (NormalObject a) a where
  sdf p (NormalObject o) = sdf p o

instance NormalC (NormalObject a) a where
  normal p (NormalObject o) = normal p o

type Map2 a = DimensionalMap 2 a a
type Map3 a = DimensionalMap 3 a a

instance Floating a => ObjectC (Map2 a) a where
  sdf p@(V3 x y _) m = sdf p . V3 x y . runMap m . toV $ V2 x y

instance Floating a => NormalC (Map2 a) a where
