{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

------------------------------------------------------
-- |
-- Module : Map.Dimensional
-- Maintainer : Milo Cress
-- Stability : Lol
-- Portability : portable
--
-- Dimensions
------------------------------------------------------
module Map.Dimension where

-- Linear
import Linear.V

-- Vector
import qualified Data.Vector as V

-- MyWorld
import Sector

-- | Same-dimension constraint. Instances of this typeclass are guaranteed to be
-- isodimensional.
class SameDimension n sh where

-- | Describes the multidimensional analog of screen-space resolution i.e. 1920x1080, etc.
type Resolution n = V n Int

-- | Constructs a 2D resolution from two "Int"s.
resolution :: Int -> Int -> Resolution 2
resolution x y = V . V.fromList $ [y, x]

-- | Converts a `Resolution` to a `Sector`
toSector :: ( Dim n
            , Fractional a
            )
         => Resolution n
         -> Sector n a
toSector s = (,) <$> (replicateV (dim s) 0) <*> s' where
  s' = fromIntegral <$> s
  replicateV :: Int -> a -> V n a
  replicateV n x = V $ V.replicate n x
