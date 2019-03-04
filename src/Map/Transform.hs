------------------------------------------------------
-- |
-- Module : Transform
-- Maintainer : Milo Cress
-- Stability : Lol
-- Portability : portable
--
-- Transformations of things
------------------------------------------------------

module Map.Transform where

import Map
-- | Though we could describe a transform as a function from Coordinate to Coordinate,
-- this is essentially the same as a parameterized 'Map'!
-- A 'TransformT' could be applied as follows:
--
-- > type Vec2 = V2 Int
-- > type Color = (Int, Int, Int)
-- > type ColorMap = Map Vec2 Color
-- >
-- > myMap :: ColorMap
-- > myMap = do
-- >   (V2 x y) <- getPoint
-- >   return (0, x `mod` 255, y `mod` 255)
-- >
-- > myTransform :: Transform Vec2
-- > myTransform = do
-- >   (V2 x y) <- getPoint
-- >   return $ V2 (x + 1) (y + 2)
-- >
-- > myMap' :: ColorMap
-- > myMap' = updateCoordinates (runTransform myTransform) myMap
type TransformT m a = Map a (m a)

-- | 'TransformT', because it is a type-synonym, has a special constructor:
transformT :: (a -> m a) -> TransformT m a
transformT = mkMap

-- | Transforms a point into another point (note that transforms cannot change dimensionality).
type Transform a = Map a a

-- | 'Transform' constructor
transform :: (a -> a) -> Transform a
transform = mkMap

-- | Like 'runTransform', but generalized for use in a monad transformer.
runTransformT :: TransformT m a -> a -> m a
runTransformT = runMap

-- | Applies a transform to a value and returns the result
runTransform :: Transform a -> a -> a
runTransform = runMap

-- | Given a transform, updates a map to use the new coordinate system.
transformCoordinates :: (Monad m)
                     => Transform c
                     -> MapT c m a
                     -> MapT c m a
transformCoordinates t = updateCoordinates (runTransform t)
