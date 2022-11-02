{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Functor law" #-}

{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where

import Control.Applicative (Alternative(empty), (<|>))
import Control.Monad.OrdCall
import Control.Monad.OrdCall.Internal
import Control.Monad.Writer (MonadWriter(tell), Writer)
import Test.Hspec
import Test.QuickCheck

reduce :: OrdCall x y a -> OrdCall x y a
reduce = \case
  App (Pure a) (Pure b) -> Pure (a b)
  x                     -> x

ordCallSize :: OrdCall x y a -> Int
ordCallSize = \case
  Pure a     -> 1
  App oc oc' -> ordCallSize oc + ordCallSize oc'
  Call x     -> 1

type IntOrdCall a = OrdCall Int Int a
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

equivOrdCall :: IntOrdCall Int -> IntOrdCall Int -> Int -> CompInt -> Property
equivOrdCall ca ca' x comp = collect (Approx $ ordCallSize $ reduce ca) $ conjoin
  [ runOrdCallM k ca === runOrdCallM k ca'
  , runOrdCallSortBy comp k ca === runOrdCallSortBy comp k ca'
  , snd (enumerateCalls 0 ca) === snd (enumerateCalls 0 ca')
  ]
  where k = logger (+ x)
infix 1 `equivOrdCall`

arbitraryInt :: Gen (IntOrdCall Int)
arbitraryInt = arbitrary

shrinkOrdCall :: IntOrdCall a -> [IntOrdCall a]
shrinkOrdCall = \case
  Pure n     -> empty
  App a b -> [Pure (a' b') | Pure a' <- a : shrinkOrdCall a, Pure b' <- b : shrinkOrdCall b]
          <|> App <$> shrinkOrdCall a <*> shrinkOrdCall b
  Call n     -> Pure <$> shrink n <|> Call <$> shrink n

instance Arbitrary (IntOrdCall Int) where
  arbitrary = frequency $ zip [1, 4, 3]
    [ Pure <$> arbitrary
    , App <$> arbitrary <*> arbitraryInt
    , Call <$> arbitrary
    ]
  shrink = shrinkOrdCall

instance Arbitrary (IntOrdCall (Int -> Int)) where
  arbitrary = oneof
    [ Pure <$> arbitrary
    , App <$> arbitrary <*> arbitraryInt
    ]
  shrink = shrinkOrdCall

instance Arbitrary (IntOrdCall (Int -> Int -> Int)) where
  arbitrary = oneof
    [ Pure <$> arbitrary
    , App <$> arbitrary <*> arbitraryInt
    ]
  shrink = shrinkOrdCall

instance Arbitrary (IntOrdCall (Int -> Int -> Int -> Int)) where
  arbitrary = Pure <$> arbitrary

instance Show x => Show (OrdCall x y a) where
  show = dbgOrdCall


{-
  Functor laws:
    fmap id = id

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
prop_fmap_id, prop_identity :: IntOrdCall Int -> Int -> CompInt -> Property
prop_fmap_id ca = id <$> ca `equivOrdCall` ca
prop_identity v = pure id <*> v `equivOrdCall` v
prop_composition :: IntOrdCall (Int -> Int) -> IntOrdCall (Int -> Int) -> IntOrdCall Int -> Int -> CompInt -> Property
prop_composition u v w = pure (.) <*> u <*> v <*> w `equivOrdCall` u <*> (v <*> w)
prop_homomorphism :: (Int -> Int) -> Int -> Int -> CompInt -> Property
prop_homomorphism f x = pure f <*> pure x `equivOrdCall` pure (f x)
prop_interchange :: IntOrdCall (Int -> Int) -> Int -> Int -> CompInt -> Property
prop_interchange u y = u <*> pure y `equivOrdCall` pure ($ y) <*> u

instance Show (a -> b) where
  show = const "<func>"
return []

main :: IO ()
main = () <$ $forAllProperties (quickCheckResult . withMaxSuccess 1000)
