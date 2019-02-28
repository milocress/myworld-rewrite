{-# LANGUAGE RecordWildCards #-}
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

import Data.List (genericLength)

traceDist :: ( ObjectC s a
             , Ord a
             , Num a
             , Fractional a
             , Floating a
             , Integral b
             ) => EngineConfig a b -> V3 a -> V3 a -> s -> Maybe a
traceDist EngineConfig{..} point ray scene = go 0 0 where
  go d n = do
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
         => EngineConfig a b -> V3 a -> V3 a -> s -> Maybe (V3 a)
traceRay conf point ray scene = do
  d <- traceDist conf point ray scene
  return $ point + ray * pure d

traceColor :: (Floating a, Epsilon a, NormalC s a, Integral b, RealFrac a, Integral c)
           => EngineConfig a b -> V3 a -> V3 a -> s -> [PointLight a] -> Maybe (V3 c)
traceColor conf@EngineConfig{..} point ray scene lights = do
  p <- traceRay conf point ray scene
  let n = normal p scene
      ambient = 0.1
      diffuse = average $ do
                  l <- lights
                  let shade = if shadowsEnabled
                              then shadow conf p n scene l
                              else 1.0
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
       => EngineConfig a b -> V3 a -> V3 a -> s -> PointLight a -> a
shadow conf@EngineConfig{..} point n scene l =
  let point' = point + n * pure minDist
  in fromMaybe 0.0 $ do
    p <- traceRay conf point' (normalize $ l - point') (scene, l)
    return $ if near conf p l then 1.0 else 0.0

near :: ( Num a
        , Floating a
        , Ord a
        )
     => EngineConfig a b -> V3 a -> V3 a -> Bool
near EngineConfig{..} v w = (abs $ distance v w) <= minDist
