{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Functor law" #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TupleSections              #-}

module Main (main) where

import Control.Applicative (Alternative(empty), (<|>))
import Control.Monad.OrdCalc
import Control.Monad.OrdCalc.Internal
import Control.Monad.Writer (MonadWriter(tell), Writer)
import Test.Hspec
import Test.QuickCheck

reduce :: OrdCalc x y a -> OrdCalc x y a
reduce = \case
  App (Pure a) (Pure b) -> Pure (a b)
  x                     -> x

calcSize :: OrdCalc x y a -> Int
calcSize = \case
  Pure a     -> 1
  App oc oc' -> calcSize oc + calcSize oc'
  Call x     -> 1

type IntCalc a = OrdCalc Int Int a
type Logger = Writer [Int]

newtype Approx
  = Approx Int
instance Show Approx where
  show (Approx n)
    | n < 10 = show n
    | n < 100 = show (n - n `mod` 10)
    | otherwise = show $ n - n `mod` 100

logger :: (Int -> Int) -> Int -> Logger Int
logger f x = tell [x] >> pure (f x)

type CompInt = Int -> Int -> Ordering

equivCalc :: IntCalc Int -> IntCalc Int -> Int -> CompInt -> Property
equivCalc ca ca' x comp = collect (Approx $ calcSize $ reduce ca) $ conjoin
  [ runCalcM k ca === runCalcM k ca'
  , runCalcSortBy comp k ca === runCalcSortBy comp k ca'
  , snd (enumerateCalls 0 ca) === snd (enumerateCalls 0 ca')
  ]
  where k = logger (+ x)
infix 1 `equivCalc`

arbitraryInt :: Gen (IntCalc Int)
arbitraryInt = arbitrary

shrinkCalc :: IntCalc a -> [IntCalc a]
shrinkCalc = \case
  Pure n     -> empty
  App a b -> [Pure (a' b') | Pure a' <- a : shrinkCalc a, Pure b' <- b : shrinkCalc b]
          <|> App <$> shrinkCalc a <*> shrinkCalc b
  Call n     -> Pure <$> shrink n <|> Call <$> shrink n

instance Arbitrary (IntCalc Int) where
  arbitrary = frequency $ zip [1, 4, 3]
    [ Pure <$> arbitrary
    , App <$> arbitrary <*> arbitraryInt
    , Call <$> arbitrary
    ]
  shrink = shrinkCalc

instance Arbitrary (IntCalc (Int -> Int)) where
  arbitrary = oneof
    [ Pure <$> arbitrary
    , App <$> arbitrary <*> arbitraryInt
    ]
  shrink = shrinkCalc

instance Arbitrary (IntCalc (Int -> Int -> Int)) where
  arbitrary = oneof
    [ Pure <$> arbitrary
    , App <$> arbitrary <*> arbitraryInt
    ]
  shrink = shrinkCalc

instance Arbitrary (IntCalc (Int -> Int -> Int -> Int)) where
  arbitrary = Pure <$> arbitrary

instance Show x => Show (OrdCalc x y a) where
  show = dbgCalc


{-
  Functor laws:
    fmap id == id

  Applicative laws:

  Identity
    pure id <*> v = v
  Composition
    pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
  Homomorphism
    pure f <*> pure x = pure (f x)
  Interchange
    u <*> pure y = pure ($ y) <*> u
-}
prop_fmap_id, prop_identity :: IntCalc Int -> Int -> CompInt -> Property
prop_fmap_id ca = id <$> ca `equivCalc` ca
prop_identity v = pure id <*> v `equivCalc` v
prop_composition :: IntCalc (Int -> Int) -> IntCalc (Int -> Int) -> IntCalc Int -> Int -> Property
prop_composition u v w = (pure (.) <*> u <*> v <*> w) `eq` (u <*> (v <*> w))
  where eq a b n = equivCalc a b n \_ _ -> EQ
prop_homomorphism :: (Int -> Int) -> Int -> Int -> CompInt -> Property
prop_homomorphism f x = pure f <*> pure x `equivCalc` pure (f x)
prop_interchange :: IntCalc (Int -> Int) -> Int -> Int -> CompInt -> Property
prop_interchange u y = u <*> pure y `equivCalc` pure ($ y) <*> u

instance Show (a -> b) where
  show = const "<func>"
return []

main :: IO ()
main = () <$ $forAllProperties (quickCheckResult . withMaxSuccess 1000)
