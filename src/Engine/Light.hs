------------------------------------------------------
-- |
-- Module : Engine.Light
-- Maintainer : Milo Cress
-- Stability : Lol
-- Portability : apparently V3 isn't portable, so...
--
-- Library for describing lights sources in "Engine" module
------------------------------------------------------
module Engine.Light where

import Linear.V3 (V3(..))

-- | A basic light, centered at a single point.
-- More complex points can be seen as distributions
-- that collapse to a single point when sampled.
type PointLight a = V3 a
