{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Engine.Trace where

import Engine.Class
import Engine.Light
import Engine.Config

import Map.PixelMap (white)

import Linear.V3 (V3(..))
import Linear.Epsilon (Epsilon)
import Linear.Metric (normalize, dot, distance)

import Control.Monad (guard)

import Data.Maybe (fromMaybe)

import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Reader (lift, ask)

import Data.List (genericLength)

traceDist :: ( ObjectC s a
             , Ord a
             , Num a
             , Fractional a
             , Floating a
             , Integral b
             ) => V3 a -> V3 a -> s -> MaybeT (Engine a b) a
traceDist point ray scene = go 0 0 where
  go d n = do
    EngineConfig{..} <- lift ask
    let dist = sdf (point + pure d * ray) scene
    guard $ n < maxSteps
    guard $ dist < maxDist
    if dist < minDist
      then return d
      else go (dist + d) (succ n)

traceRay :: ( ObjectC s a
            , Ord a
            , Fractional a
            , Floating a
            , Integral b
            )
         => V3 a -> V3 a -> s -> MaybeT (Engine a b) (V3 a)
traceRay point ray scene = do
  d <- traceDist point ray scene
  return $ point + ray * pure d

traceColor :: (Floating a, Epsilon a, NormalC s a, Integral b, RealFrac a, Integral c)
           => V3 a -> V3 a -> s -> [PointLight a] -> MaybeT (Engine a b) (V3 c)
traceColor point ray scene lights = do
  conf <- lift ask
  p <- traceRay point ray scene
  let n = normal p scene
      ambient = 0.1
      diffuse = average $ do
                  l <- lights
                  let shade = runEngine (shadow p n scene l) conf
                  return . (* shade)
                         . clamp 0 1
                         $ dot (normalize $ l - p) n
      c = fromIntegral <$> white
  return $ floor <$> clamp 0 255 <$> c * pure (ambient + diffuse)

average :: Fractional a => [a] -> a
average xs = sum xs / genericLength xs

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

shadow :: ( ObjectC s a
          , Num a
          , Fractional a
          , Ord a
          , Epsilon a
          , Floating a
          , Integral b
          )
       => V3 a -> V3 a -> s -> PointLight a -> Engine a b a
shadow point n scene l = fromMaybe 0.0 <$> (runMaybeT $ do
  EngineConfig{..} <- lift ask
  let point' = point + n * pure minDist
  p <- traceRay point' (normalize $ l - point') (scene, l)
  cond <- lift $ p `near` l
  return $ if cond then 1.0 else 0.0)

near :: ( Num a
        , Floating a
        , Ord a
        )
     => V3 a -> V3 a -> Engine a b Bool
near v w = do
  EngineConfig{..} <- ask
  return $ (abs $ distance v w) <= minDist
