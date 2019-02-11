{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module Map.PixelMap where

import Data.Array.Repa as R
import Codec.Picture
-- import qualified Data.Vector as V
import Linear.V
import Linear.V3 (V3 (..))
import Control.Monad.Identity

import Map.SectorMap
import Map.RepaMap
import Map.Dimension

import Sector
import Sector.SectorTree

-- | This is a wrapper for pixels. It's important to note that RGB8s
-- are unboxable.
type RGB8 = V3 Pixel8 --(Pixel8, Pixel8, Pixel8)

-- | This function is taken from a tutorial on Repa and JuicyPixels
-- it turns an unboxed array into an image. Pretty straightforward.
{-# INLINE toImage #-}
toImage :: Array U DIM2 RGB8 -> Image PixelRGB8
toImage a = generateImage gen width height
  where
    Z :. width :. height = R.extent a
    gen x y =
      let (V3 r g b) = a ! (Z :. x :. y)
      in PixelRGB8 r g b

-- | A specialized 'Map' that is 2-dimensional and contains 'RGB8' values.
-- This map is indexable by a 'V 2' of 'c's.
type PixelMap c = DimensionalMap 2 c RGB8

-- | Evaluates a map to an array
bakePixelMap :: ( Dim n
                , Fractional c
                , SameDimension n DIM2
                )
             => Sector n c
             -> Resolution n
             -> DimensionalMap n c RGB8
             -> DynamicImage
bakePixelMap s r m = ImageRGB8 . toImage $ runIdentity $ bakeMap s r m

writePixelMap :: ( Fractional a )
              => FilePath
              -> Resolution 2
              -> PixelMap a
              -> IO ()
writePixelMap p r m = savePngImage p (bakePixelMap (toSector r) r m)

black :: RGB8
black = V3 0 0 0

white :: RGB8
white = V3 255 255 255

toGrayScale :: Bool -> RGB8
toGrayScale True = white
toGrayScale _    = black

{-# INLINE optimizeMap #-}
optimizeMap :: ( Fractional c, Enum c, Ord c
               , Eq a, Bounded a
               , Dim n
               , Eq b, Num b )
            => DimensionalMap n c a
            -> Sector n c
            -> b
            -> DimensionalMap n c a
optimizeMap m s n = fromSectorMap minBound
                  . compileSectorTreeWith (>>>)
                  . buildSectorTree coalg
                  $ (s, n) where
  coalg (s', n') =
    let
      testSects  = subdivideSector 8 s'
      children   = subdivideSector 4 s'
      results    = fmap (sampleMidpoint m) testSects
      childSeeds = fmap (, n' - 1) children
    in case (n' == 0, same results) of
         (True, _) -> NodeF (toSectorMap (m)                     s') []
         (_, True) -> NodeF (toSectorMap (return $ head results) s') []
         _         -> NodeF (toSectorMap (return $ minBound)     s') childSeeds
  same [] = False
  same (x:xs) = all (x ==) $ xs
