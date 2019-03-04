-- {-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE ScopedTypeVariables #-}

------------------------------------------------------
-- |
-- Module : Sector
-- Maintainer : Milo Cress
-- Stability : Lol
-- Portability : portable
--
-- N-dimensional bounding boxes.
------------------------------------------------------
module Sector ( Sector
              , inSector
              , subdivideSector
              , to
              , midpoint
              ) where

import Map.Transform

import Linear.V

-- | Range type synonym, used to describe a range of values that a 'Sector' includes, in a single dimension.
type Range a = (a, a)

-- | A 'Sector' is an n-dimensional bounding-box, described by a 'V' (type-level vector) of 'Range's.
type Sector n a = V n (Range a)

-- | Checks if a given point (described by a 'V' (type-level vector)) lies inside of a sector.
inSector :: (Dim n, Ord a) => Sector n a -> V n a -> Bool
inSector s v = and $ inRange <$> s <*> v where inRange (a, b) x = a <= x && x <= b

-- | Describes a subdivisible datatype.
class Subdivide s where
  subdivide :: Int -- ^ Subdivision coefficient. Note that this number will cause different subdivision behavior depending on the datatype to which it is applied.
            -> s
            -> [s]

-- | Subdivides a range into a n sub-ranges
subdivideRange :: (Enum a, Fractional a)
               => Int -- ^ Number of sub-ranges
               -> Range a -- ^ starting range
               -> [Range a]
subdivideRange n (a, b) = zip xs (tail xs)
                          where xs = [a, a + da .. b]
                                da = (b - a) / (fromIntegral n)

instance (Enum a, Fractional a) => Subdivide (Range a) where
  subdivide = subdivideRange

-- | Divides a sector into (n ^ dim) sub-sectors, where dim is the dimension of the range.
subdivideSector :: (Enum a, Fractional a)
                => Int -- ^ Number of sub-sectors /per dimension/ (the Cartesian product causes this number to be raised to the power of the number of dimensions in the Vector).
                -> Sector n a
                -> [Sector n a]
subdivideSector n = sequence . fmap (subdivide n)

instance (Enum a, Fractional a) => Subdivide (Sector n a) where
  subdivide = subdivideSector

-- | Creates a transform given two sectors such that
-- | >> runTransform (s `to` s') s == s'
to :: (Dim n, Fractional a)
   => Sector n a
   -> Sector n a
   -> Transform (V n a)
s `to` t = transform . (<*>) $ to' <$> s <*> t where
  (a, b) `to'` (c, d) = \x -> ((x - a) / (b - a)) * (d - c) + c

-- | Returns the midpoint of an n-dimensional sector
midpoint :: (Fractional a) => Sector n a -> V n a
midpoint = fmap mid where
  mid (x, y) = (x + y) / 2

