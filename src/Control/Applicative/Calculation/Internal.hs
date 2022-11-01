{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

{- |
  This module provides full access to raw `Calc` representation. Using
  functions from it is unsafe, meaning that functor and applicative can be
  violated. For most cases, you should prefer public
  /Control.Applicative.Calculation/ interface
-}
module Control.Applicative.Calculation.Internal where
import Data.Functor.Identity (Identity(..))

-- | Calculation of @a@ with possible calls to external @x -> y@ function
data Calc x y a where
  Pure :: a -> Calc x y a
  App :: Calc x y (a -> b) -> Calc x y a -> Calc x y b
  Call :: x -> Calc x y y

instance Functor (Calc x y) where
  fmap f = App (Pure f)
instance Applicative (Calc x y) where
  pure = Pure
  Pure a <*> Pure a' = Pure $ a a'
  ca     <*> ca'     = App ca ca'

-- | Call to external function from `Calc` evaluation
call :: x -> Calc x y y
call = Call
{-# INLINE call #-}

-- | Debug the calculation.
--
-- /NB!/ The result of calling this function can
-- produce different results modulo functor and applicative laws
dbgCalc :: Show x => Calc x y a -> String
dbgCalc = \case
  Pure _     -> "Pure"
  App ca ca' -> "(App " <> dbgCalc ca <> " " <> dbgCalc ca' <> ")"
  Call x     -> "(Call " <> show x <> ")"

-- | Run calculation using a pure external function
--
-- prop> runCalc f = runIdentity . runCalcM (Identity . f)
runCalc :: (x -> y) -> Calc x y a -> a
runCalc f = runIdentity . runCalcM (Identity . f)

-- | Run calculation using an effectful external function
runCalcM :: Applicative m => (x -> m y) -> Calc x y a -> m a
runCalcM f = \case
  Pure a     -> pure a
  App cab ca -> ($) <$> runCalcM f cab <*> runCalcM f ca
  Call x     -> f x

-- | Enumerate calls of external function, replacing them by their numbers.
--
-- /NB!/ The order of produced enumeration is unpredictable!
enumerateCalls :: Enum i => i -> Calc x y a -> (Calc i y a, [(i, x)])
enumerateCalls i0 ca = let (ca', xs, _) = go i0 [] ca in (ca', xs)
  where
    go :: Enum i => i -> [(i, x)] -> Calc x y a -> (Calc i y a, [(i, x)], i)
    go i res = \case
      Pure a     -> (Pure a, mempty, i)
      App cab ca -> (App cab' ca', res'', i'')
        where
          (cab', res', i') = go i res cab
          (ca', res'', i'') = go i' res' ca
      Call x     -> (Call i, (i, x) : res, succ i)
