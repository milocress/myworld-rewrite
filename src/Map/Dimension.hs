{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Map.Dimension where

-- Linear
import Linear.V

-- Vector
import qualified Data.Vector as V

-- MyWorld
import Sector

class SameDimension n sh where

type Resolution n = V n Int

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
