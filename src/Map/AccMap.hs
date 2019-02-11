{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans -Odph -rtsopts -threaded -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -fllvm -optlo-O3 #-}
------------------------------------------------------
-- |
-- Module : RepaMap
-- Maintainer : Milo Cress
-- Stability : Lol
-- Portability : not very
--
-- Internal Representation of Maps that can turn into Repa arrays.
------------------------------------------------------
module Map.AccMap (bakeMap2, writePixelMap, Color, white, black) where

import Data.Array.Accelerate                 as A
import Data.Array.Accelerate.IO              as A
import Data.Array.Accelerate.Data.Colour.RGB as A
import Data.Array.Accelerate.LLVM.PTX (run)

import Linear.V (V, fromV, toV)
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))
import Linear.Epsilon (Epsilon (..))

import Data.Array.Accelerate.Linear.V3 () -- import orphan instances

import qualified Prelude as P
import qualified Data.Word as W

import Map (runMap, getPoint, mapMap)
import Map.Dimension (Resolution)
import Map.SectorMap (DimensionalMap)

resToSh :: V 2 P.Int -> DIM2
resToSh (fromV -> V2 x y) = Z :. x :. y

-- resToShE :: V 2 (Exp P.Int) -> Exp DIM2
-- resToShE (fromV -> V2 x y) = lift (Z :. x :. y)

shToV :: (FromIntegral P.Int a, P.Num a)
      => Exp DIM2
      -> V 2 (Exp a)
shToV (unlift -> Z :. x :. y) = toV $ V2 (fromIntegral x) (fromIntegral y)

bakeMap2 :: (Elt a, FromIntegral Int c, P.Num c)
         => Resolution 2
         -> DimensionalMap 2 (Exp c) (Exp a)
         -> Array DIM2 a
bakeMap2 r m = run . generate (constant $ resToSh r) . runMap $ do
  p <- shToV P.<$> getPoint
  P.return $ runMap m p

type PixelMap a = DimensionalMap 2 (Exp a) (Color (Exp a))

writePixelMap :: ( FromIntegral P.Int a
                 , P.Num a
                 , RealFrac a)
              => P.FilePath
              -> Resolution 2
              -> PixelMap a
              -> P.IO ()
writePixelMap path r m = writeImageToBMP path
                       $ bakeMap2 r
                       $ mapMap packRGB8
                       $ mapMap toRGB m

type Color a = V3 a

white :: (RealFrac a, P.Num a) => Color (Exp a)
white = constant P.<$> V3 255 255 255

black :: (RealFrac a, P.Num a) => Color (Exp a)
black = constant P.<$> V3 0 0 0

toRGB :: (Elt a, RealFrac a) => Color (Exp a) -> Exp (RGB W.Word8)
toRGB (V3 r g b) = lift $ RGB (floor r) (floor g) (floor b)

-- Epsilon Orphan instances
instance Epsilon (Exp Double) where
  nearZero a = P.abs a P.<= 1e-12
