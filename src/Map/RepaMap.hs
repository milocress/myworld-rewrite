{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
-- {-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans -Odph -rtsopts -threaded -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -fllvm -optlo-O3 #-}

------------------------------------------------------
-- |
-- Module : Map.RepaMap
-- Maintainer : Milo Cress
-- Stability : Lol
-- Portability : portable
--
-- Internal representation of Maps that can turn into Repa arrays.
------------------------------------------------------
module Map.RepaMap ( bakeMap ) where

-- import Data.Foldable
import qualified Data.Array.Repa as R
import Control.Monad.Identity
import Linear.V
import GHC.TypeLits
import Data.Vector as V
import Data.Vector.Unboxed as U
-- import Data.Proxy

import Map
import Map.SectorMap
import Sector
import Map.Transform
import Map.Dimension

type RepaMapT s m a = MapT s m a
type RepaMap  s   a = RepaMapT s Identity a

runRepaMapT :: RepaMapT s m a -> s -> m a
runRepaMapT = runMapT

runRepaMap :: RepaMap s a -> s -> a
runRepaMap = runMap

toArray :: s -> RepaMap s a -> R.Array R.D s a
toArray s = R.fromFunction s . runRepaMap

toRepaMap :: ( Fractional c
             , Dim n
             , R.Shape sh
             , Monad m )
          => Sector n c
          -> Resolution n
          -> DimensionalMapT n c m a
          -> RepaMapT sh m a
toRepaMap s r = changeCoordinates fromShape
              . changeCoordinates (fmap fromIntegral)
              . transformCoordinates ((toSector r) `to` s)

-- | Evaluates a "DimensionalMap" to a 2D "Array" of values.
bakeMap :: ( Dim n
           , Fractional c
           , Unbox a
           , Monad m
           , R.Shape sh )
        => Sector n c
        -> V n Int
        -> DimensionalMap n c a
        -> m (R.Array R.U sh a)
bakeMap s r = R.computeUnboxedP . toArray (toShape r) . toRepaMap s r

toShape :: ( R.Shape sh
           )
        => V n Int -> sh
toShape = R.shapeOfList . V.toList . toVector

fromShape :: ( R.Shape sh
             )
          => sh
          -> V n Int
fromShape = V . V.fromList . R.listOfShape

instance SameDimension 0 R.Z
instance (SameDimension n sh, n' ~ (n + 1)) => SameDimension n' (sh R.:. Int)
