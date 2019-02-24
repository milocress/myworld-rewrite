{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds#-}
{-# LANGUAGE RecordWildCards #-}
module Engine.Class where

import Map.SectorMap (DimensionalMap)
import Map (runMap)

import Linear.V (toV)
import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import Linear.Epsilon (Epsilon)
import Linear.Metric (normalize, norm, dot)

import Data.Function (on)

import Data.List (minimumBy)

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
      epsilon = 1e-5

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

instance (Num a, Floating a) => ObjectC (Map2 a) a where
  -- | This is an extreme oversimplification
  sdf p@(V3 x y _) m = sdf p (V3 x y (runMap m (toV $ V2 x y)))

instance (Num a, Floating a) => NormalC (Map2 a) a

data Sphere a = Sphere { sphereRadius :: a
                       , spherePos    :: V3 a
                       }

instance ( Num a
         , Floating a
         ) => ObjectC (Sphere a) a where
  sdf p Sphere{..} = norm (p - spherePos) - sphereRadius

instance (Floating a) => NormalC (Sphere a) a where
  normal p Sphere{..} = normalize $ p - spherePos

data Plane a = Plane { planePoint  :: V3 a
                     , planeNormal :: V3 a
                     }

instance (Num a) => ObjectC (Plane a) a where
  sdf p Plane{..} = dot (p - planePoint) planeNormal

instance (Num a) => NormalC (Plane a) a where
  normal _ Plane{..} = planeNormal
