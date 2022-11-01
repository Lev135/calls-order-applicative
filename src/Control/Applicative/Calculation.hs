{-# LANGUAGE TupleSections #-}
{- | This module provides a safe applicative interface for calculation
  with possible reordering of

-}
module Control.Applicative.Calculation (
  -- * `Calc` applicative data type
  Calc, call,
  -- * Simple runners
  runCalc, runCalcM,
  -- * Reordered runners
  runCalcSortOn, runCalcSortBy, runCalcSort
) where

import Control.Applicative.Calculation.Internal
import Data.Function (on)
import Data.List (sortBy)
import qualified GHC.Arr as Arr

-- | @runCalcSortBy comp f ca@ runs calculation @a@ using effectful external
-- function @f@ calling @f@ in the increasing (according to @comp@) order of
-- arguments.
--
-- /NB!/ If some @x@s are equivalent according to @comp@ calls of @f@ for them
-- should produce commutative effects
runCalcSortBy :: (Monad m, Show x) =>
  (x -> x -> Ordering) -> (x -> m y) -> Calc x y a -> m a
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
runCalcSortOn :: (Monad m, Ord x', Show x) =>
  (x -> x') -> (x -> m y) -> Calc x y a -> m a
runCalcSortOn g = runCalcSortBy (compare `on` g)


-- | @runCalcSortBy f ca@ runs calculation @a@ using effectful external
-- function @f@ calling @f@ in the increasing order of arguments.
--
-- /NB!/ If some @x@s are equal, calls of @f@ for them should produce
-- commutative effects
runCalcSort :: (Monad m, Ord x, Show x) => (x -> m y) -> Calc x y a -> m a
runCalcSort = runCalcSortBy compare
