{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GPUEngine where

import Data.Array.Accelerate as A

import Data.Array.Accelerate.Linear.V3 (V3(..))
import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.Epsilon


import qualified Prelude as P

minDist, maxDist :: ( Elt a
                    , P.Fractional a
                    ) => Exp a
minDist = constant 1e-4
maxDist = constant 1e2

maxSteps :: Exp Int
maxSteps = constant 1000

class ObjectC s a where
  sdf :: Exp (V3 a) -> s -> Exp a

instance ( Elt a
         , P.Floating (Exp a)
         ) => ObjectC (Exp (V3 a)) a where
  sdf p v = norm $ v - p

instance ( ObjectC (Exp s) a
         , Elt s
         , Elt a
         , Ord a
         ) => ObjectC (Acc (Array DIM1 s)) a where
  sdf p = the . minimum . map (sdf p)

instance ( ObjectC (Exp s) a
         , ObjectC (Exp t) a
         , Elt s
         , Elt t
         , Ord a
         ) => ObjectC (Exp (s, t)) a where
  sdf p (unlift -> (a, b)) = min (sdf p (a :: Exp s)) (sdf p (b :: Exp t)) where

class ObjectC s a => NormalC s a where
  normal :: ( P.Floating (Exp a)
            , Epsilon a
            , P.Num a
            , P.Fractional a
            ) => Exp (V3 a) -> s -> Exp (V3 a)
  normal p scene = normalize $ lift $ V3
    (de (p + x * epsilon) - de (p - x * epsilon))
    (de (p + y * epsilon) - de (p - y * epsilon))
    (de (p + z * epsilon) - de (p - z * epsilon))
    where
      de = P.flip sdf scene
      x = lift $ constant P.<$> V3 1 0 0
      y = lift $ constant P.<$> V3 0 1 0
      z = lift $ constant P.<$> V3 0 0 1
      epsilon = lift $ V3 minDist minDist minDist

instance ( NormalC (Exp s) a
         , Elt s
         , Ord a
         ) => NormalC (Acc (Array DIM1 s)) a where
  normal p = normal p . the . minimumOn (sdf p) where
    minimumOn f xs = fold1 minBy xs where
      minBy x y = f y > f x ? (x, y)

-- traceDist point ray scene = while
--   (\(unlift -> (i, r)) -> ( let d = fromMaybe minDist r in d > minDist && isJust r))
--   (\(i, d) ->  (i + 1, ))
