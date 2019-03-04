{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds#-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

------------------------------------------------------
-- |
-- Module : Engine.Class
-- Maintainer : Milo Cress
-- Stability : Lol
-- Portability : who even knows
--
-- Typeclasses and instances of internal "Engine" module raymarching helpers, such as minimum distance estimators.
------------------------------------------------------
module Engine.Class (
    ObjectC (..)
  , NormalC (..)
  , Object (..)
  , NormalObject (..)
  , GradMapInfo (..)
  -- , GradMapInfo (..)
  , Map2
  , GradMap2
  -- , GradMap2
  , Sphere (..)
  , Plane (..)
  ) where

import Map (Map, runMap, getPoint)

import Linear.V2 (V2(..))
import Linear.V3 (V3(..), cross)
import Linear.Epsilon (Epsilon)
import Linear.Metric (normalize, norm, dot, distance)

import Data.Function (on)

import Data.List (minimumBy)

-- | A distance-estimated, ray-marchable object defines a "nearestPoint" function
-- | and a standard minimum distance function
class ObjectC s a where
  nearestPoint :: V3 a -> s -> V3 a
  sdf :: V3 a -> s -> a

-- | "ObjectC" can be extended to describe diffuse light interaction by defining its normal at a given point.
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
      epsilon = 1e-5

instance Floating a => ObjectC (V3 a) a where
  sdf p v = distance p v
  nearestPoint _ = id

instance ( ObjectC s a
         , Ord a
         ) => ObjectC [s] a where
  sdf p = minimum . fmap (sdf p)
  nearestPoint p os = nearestPoint p $ minimumBy (compare `on` sdf p) os

instance ( NormalC s a
         , Ord a
         ) => NormalC [s] a where
  normal p = normal p . minimumBy (compare `on` sdf p)

instance ( ObjectC s a
         , ObjectC t a
         , Ord a, Floating a
         ) => ObjectC (s, t) a where
  sdf p (a, b) = min (sdf p a) (sdf p b)
  nearestPoint p (a, b) = nearestPoint p [nearestPoint p a, nearestPoint p b]

-- | Existential object type for heterogeneous collections
data Object a = forall s . ObjectC s a => Object s
-- | Existential normal type for heterogeneous collections
data NormalObject a = forall s . NormalC s a => NormalObject s

instance ObjectC (Object a) a where
  sdf p (Object o) = sdf p o
  nearestPoint p (Object o) = nearestPoint p o

instance ObjectC (NormalObject a) a where
  sdf p (NormalObject o) = sdf p o
  nearestPoint p (NormalObject o) = nearestPoint p o

instance NormalC (NormalObject a) a where
  normal p (NormalObject o) = normal p o

data DualMapInfo a = DualMapInfo { getDValue :: a
                                 , getNormal :: V3 a
                                 }

-- | The "Map" analog of a Dual number, which contains a value and a derivative
data GradMapInfo a = GradMapInfo { getGValue :: a
                                 , getGrad :: V2 a
                                 }

-- | Type synonym for a 2D map
type Map2 a = Map (V2 a) a

type DualMap2 a = Map (V2 a) (DualMapInfo a)

-- | "Map2" of Dual numbers
type GradMap2 a = Map (V2 a) (GradMapInfo a)

demote :: V3 a -> V2 a
demote (V3 x y _) = V2 x y
{-# INLINE demote #-}

promote :: Num a => V2 a -> V3 a
promote (V2 x y) = V3 x y 0
{-# INLINE promote #-}

toDualMap :: (Floating a, Epsilon a) => GradMap2 a -> DualMap2 a
toDualMap m = do
  p <- getPoint
  let (GradMapInfo val g) = runMap m p
      -- Normal
      dz = g `dot` normalize g
      z  = V3 0 0 1
      g3 = normalize . promote $ g
      n = normalize $ cross
        (g3 + pure dz * z)
        (cross z g3)
      -- Nearest Point
      -- I got nothing on this. The type system is just killing me
  return $ DualMapInfo val n
{-# INLINE toDualMap #-}

instance (Floating a, Ord a) => ObjectC (DualMap2 a) a where
  -- | Again, this is an extreme oversimplification
  -- sdf p m = sdf p . getDNearestPoint (runMap m $ demote p) $ p
  sdf p m = sdf p $ nearestPoint p m
  nearestPoint p m = estimateNearestPoints p m e !! 0 where
    p'@(V2 a b) = demote p
    e = V3 a b $ getDValue $ runMap m p'

estimateNearestPoints :: Num a => V3 a -> DualMap2 a -> V3 a -> [V3 a]
estimateNearestPoints point mymap estimate = estimate : go point mymap estimate where
  go p m e = e' : estimateNearestPoints p m e' where
    e' = betterEstimate p m e

betterEstimate :: Num a => V3 a -> DualMap2 a -> V3 a -> V3 a
betterEstimate p m e = nearestPoint p $ Plane e' getNormal where
  DualMapInfo{..} = runMap m $ demote e
  (V3 a b _) = e
  e' = (V3 a b $ getDValue)

instance (Floating a, Ord a) => NormalC (DualMap2 a) a where
  normal p m = getNormal
             . runMap m
             $ demote p' where
    -- p' = nearestPoint p m
    p' = p

instance (Floating a, Ord a, Epsilon a) => ObjectC (GradMap2 a) a where
  sdf p m = sdf p (toDualMap m)
  nearestPoint p m = nearestPoint p (toDualMap m)

instance (Floating a, Ord a, Epsilon a) => NormalC (GradMap2 a) a where
  normal p m = normal p (toDualMap m)

-- | Distance estimated, ray-marchable sphere
data Sphere a = Sphere { sphereRadius :: a
                       , spherePos    :: V3 a
                       }
instance ( Num a
         , Floating a
         ) => ObjectC (Sphere a) a where
  sdf p Sphere{..} = norm (p - spherePos) - sphereRadius
  nearestPoint p Sphere{..} = v - v * pure sphereRadius where v = (p - spherePos)

instance (Floating a) => NormalC (Sphere a) a where
  normal p Sphere{..} = normalize $ p - spherePos

-- | Distance estimated, ray-marchable plane
data Plane a = Plane { planePoint  :: V3 a
                     , planeNormal :: V3 a
                     }

instance (Num a) => ObjectC (Plane a) a where
  sdf p Plane{..} = dot (p - planePoint) planeNormal
  nearestPoint p plane = p + dir * pure dist where
    Plane{..} = plane
    dir = -planeNormal
    dist = sdf p plane

instance (Num a) => NormalC (Plane a) a where
  normal _ Plane{..} = planeNormal
