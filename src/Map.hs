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
           , mapMap
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

-- | Returns the point, which is the Map's read-only environment.
getPoint :: Monad m => MapT c m c
getPoint = ask

-- | Takes a 'Transform' and applies it to a map, effectively changing the location of its coordinates.
updateCoordinates :: (Monad m)
                  => (c -> c) -- ^ Unwrapped transform (since transform is an extension of the 'Reader' monad, the unwrapped data is a function)
                  -> MapT c m a
                  -> MapT c m a
updateCoordinates = local

-- | Similar to updateCoordinates, but capable of changing the type of the "Map" this essentially facilitates
-- | changing coordinate systems (from 2- to 3-dimensions, for example).
changeCoordinates :: (c' -> c)
                  -> MapT c m a
                  -> MapT c' m a
changeCoordinates = withReaderT

-- | Changes the underlying monad of a 'MapT'
mapMapT :: (m a -> n b) -> MapT c m a -> MapT c n b
mapMapT = mapReaderT

-- | Applies a function to the result of a map at a given point
-- | essentially chains a function to the end of the "Reader" monad.
mapMap :: (a -> b) -> Map c a -> Map c b
mapMap = fmap
