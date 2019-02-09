-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PolyKinds #-}
module Sector.SectorTree where

import Map
import Map.SectorMap

-- import Control.Monad
import Data.Functor.Foldable

data TreeF a r = NodeF a [r] deriving (Functor, Show)

type Tree a = Fix (TreeF a)

type SectorTree n c a = Tree (SectorMap n c a)

{-# INLINE compileSectorTreeWith #-}
-- | Takes a 'SectorTree' and returns it with each layer combined with the layer above according to
-- a given binary function. This implementation now runs in logarithmic time rather than linear.
compileSectorTreeWith :: (SectorMap n c a -> SectorMap n c a -> SectorMap n c a) -- ^The binary function
                      -- to apply to 'SectorTree's on various levels of the hierarchy.
                      -> SectorTree n c a -- ^ The SectorTree to compile
                      -> SectorMap n c a -- ^ The resulting 'SectorMap'
compileSectorTreeWith (<?>) = cata alg where
  alg (NodeF m []) = m
  alg (NodeF m ms) = do
    point <- getPoint
    -- guard $ inSectorMap m point -- The sub-sector must contain the point (this is guaranteed by the next guard)
    (x:_) <- return $ filter (flip inSectorMap point) ms -- Check that at least 1 sub-sector contains the point
    x <?> m -- combine the maps
    -- We assume that fail == empty

    -- There's an interesting debate on whether to use fail method of do-notation or to use the guard function
    -- to weed out points not in the SectorMap. I've opted for a hybrid solution, which preserves the elegance
    -- and simplicity of the code to the greatest extent. The first expression uses a guard, because the
    -- block doesn't need to return anything of use to us later on. The second expression uses an incomplete
    -- pattern match, which desugars into a complete case-split which calls the fail function for SectorMap
    -- if the pattern match doesn't succeed (-XMonadFailDesugaring) is enabled in 8.6.
    -- The alternative would be the following:
    --
    -- let xs = filter (flip inSectorMap point) ms
    -- guard . not . null $ xs
    -- m <?> head xs
    --
    -- This is, in my opinion, much less desirable for 3 reasons:
    -- - First, we assign a variable binding to the entire list, which is unnecessary as we only
    --   the first element.
    -- - Second, the guard statement takes up an entire line, and requires an Alternative instance.
    -- - Third, the <?> function is now applied to m and 'head xs,' rather than simply being applied
    --   to "x."
    --
    -- The current implementation is not perfect either. Needing to apply 'return' to the 'filter' expression
    -- in order for it to play nicely with the automatic case-splitting seems like unnecessary overhead.
    -- A third option is allows us to bind only to x without needing to pack the 'filter' expression into a monad:
    -- case filter (flip inSectorMap point) ms of
    --     (a:_) -> m <?> a
    --     _     -> empty -- (or 'fail')
    --
    -- This solution is silly for a few reasons.
    -- - I'm rewriting logic here that is automatically defined for me with the fail method of do-notation.
    -- - This takes 2 lines, as many as the 'guard' solution.
    -- - Another level of indentation implies a greater degree of complexity than is actually present.

{-# INLINE buildSectorTree #-}
-- | This is an anamorphism.
buildSectorTree :: (s -> TreeF (SectorMap n c a) s) -> s -> SectorTree n c a
buildSectorTree = ana
