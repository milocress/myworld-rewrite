{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Engine.Config where

import Control.Monad.Reader

data EngineConfig a b = EngineConfig { minDist :: a
                                     , maxDist :: a
                                     , maxSteps :: b
                                     , shadowsEnabled :: Bool
                                     }

newtype Engine b c a = Engine (Reader (EngineConfig b c) a) deriving
  (Functor, Applicative, Monad, MonadReader (EngineConfig b c))

runEngine :: Engine b c a -> EngineConfig b c -> a
runEngine (Engine r) c = runReader r c
