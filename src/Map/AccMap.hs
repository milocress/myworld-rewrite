{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
------------------------------------------------------
-- |
-- Module : RepaMap
-- Maintainer : Milo Cress
-- Stability : Lol
-- Portability : not very
--
-- Internal Representation of Maps that can turn into Repa arrays.
------------------------------------------------------
module Map.AccMap (bakeMap2) where

import Data.Array.Accelerate                 as A
import Data.Array.Accelerate.IO              as A
import Data.Array.Accelerate.Data.Colour.RGB as A
import Data.Array.Accelerate.LLVM.PTX (run)

import qualified Prelude as P

import Linear.V (V, fromV, toV)
import Linear.V2 (V2 (..))

import Map (runMap, getPoint, mapMap, Map)
import Map.Dimension (Resolution)
import Map.SectorMap (DimensionalMap)

resToSh :: V 2 P.Int -> DIM2
resToSh (fromV -> V2 x y) = Z :. x :. y

-- resToShE :: V 2 (Exp P.Int) -> Exp DIM2
-- resToShE (fromV -> V2 x y) = lift (Z :. x :. y)

shToV :: (FromIntegral P.Int a, P.Num a) => Exp DIM2 -> V 2 (Exp a)
shToV (unlift -> Z :. x :. y) = toV $ V2 (fromIntegral x) (fromIntegral y)

bakeMap2 :: (Elt a, FromIntegral Int c, P.Num c) => Resolution 2 -> DimensionalMap 2 (Exp c) (Exp a) -> Array DIM2 a
bakeMap2 r m = run . generate (constant $ resToSh r) . runMap $ do
  p <- shToV P.<$> getPoint
  P.return $ runMap m p

type PixelMap c = DimensionalMap 2 (Exp c) (Exp (RGB Word8))

writePixelMap :: (FromIntegral P.Int c, P.Num c) => P.FilePath -> Resolution 2 -> PixelMap c -> P.IO ()
writePixelMap path r m = writeImageToBMP path
                       $ bakeMap2 r
                       $ mapMap packRGB8 m
