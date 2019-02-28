{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
module Engine.Config where

import Control.Monad.Reader

import Engine.Light

data EngineConfig a b = EngineConfig { minDist :: a
                                     , maxDist :: a
                                     , maxSteps :: b
                                     , shadowsEnabled :: Bool
                                     }

data EngineState a s = EngineState { scene :: s
                                   , lights :: [PointLight a]
                                   }

newtype Engine b c s a = Engine (Reader (EngineConfig b c, EngineState b s) a) deriving
  (Functor, Applicative, Monad, MonadReader (EngineConfig b c, EngineState b s))

runEngine :: Engine b c s a -> EngineConfig b c -> EngineState b s -> a
runEngine (Engine r) c s = runReader r (c, s)

getConfig :: Engine b c s (EngineConfig b c)
getConfig = fst <$> ask

getState :: Engine b c s (EngineState b s)
getState = snd <$> ask

getLights :: Engine b c s ([PointLight b])
getLights = lights <$> getState

getScene :: Engine b c s s
getScene = scene <$> getState
