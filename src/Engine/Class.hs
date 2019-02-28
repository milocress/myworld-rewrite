{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds#-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Engine.Class where

import Map (Map, runMap, getPoint)

import Linear.V2 (V2(..))
import Linear.V3 (V3(..), cross)
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


data DualMapInfo a = DualMapInfo { getDValue :: a
                                 , getNormal :: V3 a
                                 , getNearestPoint :: V3 a -> V3 a
                                 }

data GradMapInfo a = GradMapInfo { getGValue :: a
                                 , getGrad :: V2 a
                                 }

type Map2 a = Map (V2 a) a
type DualMap2 a = Map (V2 a) (DualMapInfo a)

type GradMap2 a = Map (V2 a) (GradMapInfo a)

demote :: V3 a -> V2 a
demote (V3 x y _) = V2 x y
promote :: Num a => V2 a -> V3 a
promote (V2 x y) = V3 x y 0

toDualMap :: (Num a, Floating a, Epsilon a) => GradMap2 a -> DualMap2 a
toDualMap m = do
  p@(V2 u v) <- getPoint
  let (GradMapInfo val g) = runMap m p
      dz = g `dot` normalize g
      z  = V3 0 0 1
      g3 = normalize . promote $ g
      n = normalize $ cross
        (g3 + pure dz * z)
        (cross z g3)
  return $ DualMapInfo val n (const $ V3 u v val)

instance (Floating a, Ord a) => ObjectC (DualMap2 a) a where
  -- | Again, this is an extreme oversimplification
  sdf p m = sdf p . getNearestPoint (runMap m $ demote p) $ p

instance (Floating a, Ord a) => NormalC (DualMap2 a) a where
  normal p m = getNormal
             . runMap m
             $ demote p

instance (Floating a, Ord a, Epsilon a) => ObjectC (GradMap2 a) a where
  sdf p m = sdf p (toDualMap m)

instance (Floating a, Ord a, Epsilon a) => NormalC (GradMap2 a) a where
  normal p m = normal p (toDualMap m)

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
