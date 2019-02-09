------------------------------------------------------
-- |
-- Module : Map
-- Maintainer : Milo Cress
-- Stability : Lol
-- Portability : portable
--
-- Functions are just Maps between sets, right?
------------------------------------------------------

module Map ( MapT
           , Map
           , mapT
           , mkMap
           , runMapT
           , runMap
           , getPoint
           , updateCoordinates
           , changeCoordinates
           , mapMapT
           ) where

import Control.Monad.Reader
import Control.Monad.Identity

-- | The 'MapT' datatype is a wrapper of the 'ReaderT' monad.
type MapT    = ReaderT

-- | The 'Map' type is a specialization of the 'MapT' and the 'Identity' monad.
type Map c a = MapT c Identity a

-- | constructor for 'MapT' datatype
mapT :: (c -> m a) -> MapT c m a
mapT = ReaderT

-- | constructor for 'Map' datatype ('map' is already defined in the Prelude module)
mkMap :: (c -> a) -> Map c a
mkMap = reader

-- | The 'runMapT' function calculates the value of a 'MapT' at a given point.
runMapT :: MapT c m a -- ^ The 'MapT' whose attribute we want
        -> c          -- ^ The point on the 'MapT' whose attribute we want
        -> m a        -- ^ The attribute
runMapT = runReaderT

-- | The 'runMap' function calculates the value of a 'Map' at a given point.
-- It is essentially the same as the runMapT function specialized to the 'Identity' monad.
runMap :: Map c a -- ^ The 'Map' whose attribute we want
       -> c       -- ^ The point on the 'Map' whose attribute we want.
       -> a       -- ^ The attribute
runMap m = runIdentity . runMapT m

getPoint :: Monad m => MapT c m c
getPoint = ask

-- | Takes a 'Transform' and applies it to a map, effectively changing the location of its coordinates.
updateCoordinates :: (Monad m)
                  => (c -> c) -- ^ Unwrapped transform (since transform is an extension of the 'Reader' monad, the unwrapped data is a function)
                  -> MapT c m a
                  -> MapT c m a
updateCoordinates = local

changeCoordinates :: (Monad m)
                  => (c' -> c)
                  -> MapT c m a
                  -> MapT c' m a
changeCoordinates = withReaderT

-- | Changes the underlying monad of a 'MapT'
mapMapT :: (m a -> n b) -> MapT c m a -> MapT c n b
mapMapT = mapReaderT
