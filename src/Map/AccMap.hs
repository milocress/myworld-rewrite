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
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

------------------------------------------------------
-- |
-- Module : RepaMap
-- Maintainer : Milo Cress
-- Stability : Lol
-- Portability : portable
--
-- Internal Representation of Maps that can turn into Repa arrays.
------------------------------------------------------
module Map.AccMap where

-- import Data.Foldable
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Array.Sugar as A
import qualified Data.Array.Accelerate.Interpreter as I
-- import qualified Data.Array.Accelerate.LLVM.PTX as GPU
import Control.Monad.Identity

import Linear.V
import GHC.TypeLits
import Data.Vector as V
-- import Data.Proxy

import Map
-- import SectorMap
-- import Sector
-- import Transform
import Map.Dimension

type AccMapT sh m a = MapT (A.Exp sh) m (A.Exp a)
type AccMap  sh   a = AccMapT sh Identity a

runAccMapT :: (A.Shape sh) => AccMapT sh m a -> A.Exp sh -> m (A.Exp a)
runAccMapT = runMapT

runAccMap :: (A.Shape sh) => AccMap sh a -> A.Exp sh -> A.Exp a
runAccMap = runMap


toAcc :: ( A.Shape sh
         , A.Elt   a)
      => sh
      -> AccMap sh a
      -> A.Acc (A.Array sh a)
toAcc sh m = A.generate (A.constant sh) $ runAccMap m

-- toAccMap :: Sector n c
--          -> Resolution n
--          -> DimensionalMap n (A.Exp c) (A.Exp a)
--          -> AccMap c a
-- toAccMap s r = changeCoordinates A.unlift
--              . changeCoordinates fromShape
--              -- . changeCoordinates fromIntegral
--              . transformCoordinates ((toSector r) `to` s)

-- bakeMap :: ( SameDimension n sh
--            , A.Shape sh
--            , Fractional c
--            , Dim n
--            )
--         => Sector n c
--         -> Resolution n
--         -> DimensionalMap n (A.Exp c) (A.Exp a)
--         -> A.Array sh a
-- bakeMap = _

-- | Fuck the accelerate library, I mean seriously guys why you gotta make everything so hard?
bakeAccMap :: ( A.Shape sh
              , A.Elt a
              , SameDimension n sh )
           -- => Sector n c
           => Resolution n
           -> AccMap sh a
           -> A.Array sh a
bakeAccMap r m = I.run $ toAcc (toShape r) m

toShape :: ( A.Shape sh
           , SameDimension n sh
           )
        => (V n Int)
        -> sh
toShape = A.listToShape . V.toList . toVector

fromShape :: (A.Shape sh, SameDimension n sh)
          => sh
          -> V n Int
fromShape = V . V.fromList . A.shapeToList

instance SameDimension 0 A.Z
instance (SameDimension n sh, n' ~ (n + 1)) => SameDimension n' (sh A.:. Int)
