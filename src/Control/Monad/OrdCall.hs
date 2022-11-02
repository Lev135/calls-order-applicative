{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{- | This module provides a safe applicative interface for calculation
  with possible reordering of external effectful function's calls
-}
module Control.Monad.OrdCall (
  -- * `OrdCall` applicative data type
  OrdCall, call,
  -- * Simple runners
  runCalc, runCalcM,
  -- * Reordered runners
  runCalcSortOn, runCalcSortBy, runCalcSort
) where

import Control.Monad.OrdCall.Internal
import Data.Function (on)
import Data.List (sortBy)
import Debug.Trace (traceM)
import qualified GHC.Arr as Arr

-- | @runCalcSortBy comp f ca@ runs calculation @a@ using effectful external
-- function @f@ calling @f@ in the increasing (according to @comp@) order of
-- arguments.
--
-- /NB!/ If some @x@s are equivalent according to @comp@ calls of @f@ for them
-- should produce commutative effects
runCalcSortBy :: Monad m =>
  (x -> x -> Ordering) -> (x -> m y) -> OrdCall x y a -> m a
runCalcSortBy comp f ca = do
  let (ca', imxs) = enumerateCalls (0 :: Int) ca
      xs = sortBy (comp `on` snd) imxs
  ys <- traverse (\(k, x) -> (k, ) <$> f x) xs
  let imys = Arr.array (0, length ys - 1) ys
  pure $ runCalc (imys Arr.!) ca'

-- | @runCalcSortBy g f ca@ runs calculation @a@ using effectful external
-- function @f@ calling @f@ in the increasing (on @g@) order of arguments.
--
-- /NB!/ If @g@ maps some @x@s to equal elements, calls of @f@ for them
-- should produce commutative effects
runCalcSortOn :: (Monad m, Ord x') =>
  (x -> x') -> (x -> m y) -> OrdCall x y a -> m a
runCalcSortOn g = runCalcSortBy (compare `on` g)


-- | @runCalcSortBy f ca@ runs calculation @a@ using effectful external
-- function @f@ calling @f@ in the increasing order of arguments.
--
-- /NB!/ If some @x@s are equal, calls of @f@ for them should produce
-- commutative effects
runCalcSort :: (Monad m, Ord x) =>
  (x -> m y) -> OrdCall x y a -> m a
runCalcSort = runCalcSortBy compare
