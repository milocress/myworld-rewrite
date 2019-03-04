-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
------------------------------------------------------
-- |
-- Module : Map.SectorMap
-- Maintainer : Milo Cress
-- Stability : Lol
-- Portability : portable
--
-- Bounded Maps, or maps that can fail (spectacularly)
------------------------------------------------------
module Map.SectorMap ( DimensionalMapT
                     , DimensionalMap
                     , SectorMap
                     , runSectorMap
                     , dimensionalMap
                     , sectorMap
                     , (<+>)
                     , (>>>)
                     , toSectorMap
                     , fromDimensionalMap
                     , fromSectorMap
                     , emptySectorMap
                     , inSectorMap
                     , sampleMidpoint
                     ) where

import Map
import Sector

import Control.Applicative
import Control.Monad.Identity
import Data.Maybe
import Linear.V

-- | Extends 'MapT' with parameterized dimensionality.
--   I feel like some kind of Idris programmer.
type DimensionalMapT n c m a = MapT (V n c) m a
-- | Analogous to a 'Map' with parameterized dimensionality,
--   or a 'DimensionalMap' specialized to the 'Identity' monad.
type DimensionalMap n c a = DimensionalMapT n c Identity a

-- | Constructs a "DimensionalMap" from a function.
dimensionalMap :: (V n c -> a) -> DimensionalMap n c a
dimensionalMap = mkMap

-- | SectorMaps are maps that have bounds and can fail.
type SectorMap n c a = DimensionalMapT n c Maybe a

-- | Creates a 'SectorMap' from a function that can fail.
sectorMap :: (V n c -> Maybe a) -> SectorMap n c a
sectorMap = mapT

-- | I hate creating Orphan instances, but this one is necessary in order
--   for my union overlay functions to play nicely with 'Map's, which run in
--   the 'Identity' monad.
instance Alternative Identity where
  (<|>) = const
  empty = undefined

-- | Union additive overlay of two maps
{-# SPECIALIZE (<+>) :: Num a => SectorMap n c a -> SectorMap n c a -> SectorMap n c a #-}
(<+>) :: (Alternative f, Num a) => f a -> f a -> f a
bot <+> top = (+) <$> (top <|> pure 0) <*> (bot <|> pure 0)

{-# SPECIALIZE (>>>) :: SectorMap n c a -> SectorMap n c a -> SectorMap n c a #-}
-- | Union overlay of two maps
(>>>) :: (Alternative f) => f a -> f a -> f a
(>>>) = (<|>)

-- | Converts a 'DimensionalMap' to a 'SectorMap' bounded by the
-- input 'Sector'.
toSectorMap :: (Dim n, Ord c)
            => DimensionalMap n c a
            -> Sector n c
            -> SectorMap n c a
toSectorMap m s = do
  point <- getPoint
  guard $ inSector s point
  mapMapT (Just . runIdentity) m

-- | Turns a "DimensionalMap" into a "SectorMap" with an infinite boundary.
fromDimensionalMap :: DimensionalMap n c a
                   -> SectorMap n c a
fromDimensionalMap = mapMapT (Just . runIdentity)

-- | Given a fallback value, turns a "SectorMap" into a "DimensionalMap".
-- The "Map" equivalent of "fromMaybe".
fromSectorMap :: a -- ^ Fallback value, should the 'SectorMap'
              -- return 'Nothing' at a given point.
              -> SectorMap n c a -- ^ The 'SectorMap'
              -- on which we'll perform the computation.
              -> DimensionalMap n c a -- ^ The resulting n-dimensional map,
              -- which is well-defined at every conceivable point in nD space.
fromSectorMap fallback = mapMapT $ Identity . fromMaybe fallback

-- | The identity value for '(\<|\>)' and '(>>>)'
emptySectorMap :: SectorMap n c a
emptySectorMap = empty

-- | Evaluates a "sectorMap" to a value wrapped in the "Maybe" monad
runSectorMap :: SectorMap n c a -> V n c -> Maybe a
runSectorMap = runMapT

-- | Checks if a given point is inside of a "sectorMap"
inSectorMap :: SectorMap n c a -> V n c -> Bool
inSectorMap m = isJust . runSectorMap m

-- | Returns the value of a map at the center of a sector.
-- Note that the map must be a "DimensionalMap" as
-- opposed to a "SectorMap" in order for the return value to
-- be in the identity monad.
sampleMidpoint :: (Fractional c)
               => DimensionalMap n c a
               -> Sector n c
               -> a
sampleMidpoint m sec = runMap m $ midpoint sec
