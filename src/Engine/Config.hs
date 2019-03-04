{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

------------------------------------------------------
-- |
-- Module : Engine.Config
-- Maintainer : Milo Cress
-- Stability : Lol
-- Portability : who even knows
--
-- Core datatypes for "Engine" module.
------------------------------------------------------

module Engine.Config where

import Control.Monad.Reader

import Engine.Light

-- | Describes the configuration of a raymarching engine object.
-- The options "minDist", "maxDist", "maxSteps", and "shadowsEnabled"
-- are performance-related, and control the number of iterations
-- and recursion depth of the ray-tracing functions in "Engine.Trace".
data EngineConfig a b = EngineConfig { minDist :: a
                                     , maxDist :: a
                                     , maxSteps :: b
                                     , shadowsEnabled :: Bool
                                     }

-- | This describes the state of a raymarching engine scene, including
-- the objects in it (which must be instances of "NormalC") and the lights that
-- illuminate it.
data EngineState a s = EngineState { scene :: s
                                   , lights :: [PointLight a]
                                   }

-- | The core "Engine" monad, which wraps the state and configuration of a scene
-- in the "Reader" monad.
newtype Engine b c s a = Engine (Reader (EngineConfig b c, EngineState b s) a) deriving
  (Functor, Applicative, Monad, MonadReader (EngineConfig b c, EngineState b s))

-- | Runs an engine given a configuration and state.
runEngine :: Engine b c s a -> EngineConfig b c -> EngineState b s -> a
runEngine (Engine r) c s = runReader r (c, s)

-- | Returns the configuration of an "Engine"
getConfig :: Engine b c s (EngineConfig b c)
getConfig = fst <$> ask

-- | Returns the state of an "Engine"
getState :: Engine b c s (EngineState b s)
getState = snd <$> ask

-- | Returns a list of lights in an Engine's state
getLights :: Engine b c s ([PointLight b])
getLights = lights <$> getState

-- | Returns the objects in an Engine's state
getScene :: Engine b c s s
getScene = scene <$> getState
