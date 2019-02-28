module Engine.Config where

data EngineConfig a b = EngineConfig { minDist :: a
                                     , maxDist :: a
                                     , maxSteps :: b
                                     , shadowsEnabled :: Bool
                                     }
