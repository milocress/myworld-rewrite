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

import Map (Map, runMap)

-- import Linear.V (toV, fromV)
import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import Linear.Epsilon (Epsilon)
-- import Linear.Conjugate (Conjugate)
import Linear.Metric (normalize, norm, dot)
-- import Linear.Quaternion (rotate, axisAngle)

-- import Numeric.AD (grad)
-- import Numeric.AD.Mode.Reverse (Reverse)

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

type Map2 a = Map (V2 a) a
type DualMap2 a = Map (V2 a) (a, V3 a)

-- instance (Floating a) => ObjectC (Map2 a) a where
--   -- | This is an extreme oversimplification
--   sdf p@(V3 x y _) m = sdf p p' where
--     z  = runMap m (V2 x y)
--     p' = V3 x y z

instance (Floating a, Ord a) => ObjectC (DualMap2 a) a where
  -- | Again, this is an extreme oversimplification
  sdf p@(V3 x y _) m = sdf p p' where
    mapAt (V3 a b _) = (V3 a b (fst $ runMap m (V2 a b)))
    pointCloud point r = promote <$> do
      r' <- [r, r / 2, r / 32]
      [
          point
        , point + V2 0 r'
        , point + V2 0 (-r')
        , point + V2 r' 0
        , point + V2 (-r') 0
        ]
    promote (V2 a b) = (V3 a b 0)
    z  = fst $ runMap m (V2 x y)
    -- p' = mapAt <$> pointCloud (V2 x y) z
    p' = mapAt p

instance (Floating a, Ord a) => NormalC (DualMap2 a) a where
  normal (V3 x y _) m = snd $ runMap m (V2 x y)

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
