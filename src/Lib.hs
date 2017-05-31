{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

import Control.Lens (Field1, Field2, Lens, Lens', ALens', _1, _2, cloneLens)
import Control.Lens.Internal.Context

someFunc :: IO ()
someFunc = putStrLn "someFunc"

tuple :: (Int, Int)
tuple = (1, 2)

---------------------------------------------------------------------

incrementThroughExpandLens
  :: (forall f. Functor f => (Int -> f Int) -> (Int, Int) -> f (Int, Int))
  -> IO (Int, Int)
incrementThroughExpandLens l = l (\b -> b + 1 <$ print b) tuple

incrementThroughExpandLens' :: ALens' (Int, Int) Int -> IO (Int, Int)
incrementThroughExpandLens' l = cloneLens l (\b -> b + 1 <$ print b) tuple

-- testIncrementThrough :: Bool -> IO (Int, Int)
-- testIncrementThrough = incrementThroughExpandLens `comp` bFuncExpandLens

testIncrementThrough' :: Bool -> IO (Int, Int)
testIncrementThrough' = incrementThroughExpandLens' `comp'` bFuncExpandLens

bFuncExpandLens
  :: forall f.
     Functor f
  => Bool -> (Int -> f Int) -> (Int, Int) -> f (Int, Int)
bFuncExpandLens b = if b then _1 else _2

-- comp
--   :: ((forall g. Functor g => (Int -> g Int) -> (Int, Int) -> g (Int, Int)) -> IO (Int, Int))
--   -> (forall f. Functor f => Bool -> (Int -> f Int) -> (Int, Int) -> f (Int, Int))
--   -> Bool
--   -> IO (Int, Int)
-- comp = (.)

comp'
  :: (ALens' (Int, Int) Int -> IO (Int, Int))
  -- -> (Bool -> (Int -> (Pretext (->) Int Int) Int) -> (Int, Int) -> (Pretext (->) Int Int) (Int, Int))
  -> (forall f. Functor f => Bool -> (Int -> f Int) -> (Int, Int) -> f (Int, Int))
  -> Bool
  -> IO (Int, Int)
comp' = (.)

-------------------------------------------------------------------------------

-- A similar example but a little simpler:

data Applicativeable =
  Applicativeable { unApplicativeable :: forall f. Applicative f => f Int }

fInt :: (forall g. Applicative g => g Int) -> IO Int
fInt = fmap (+ 1)

fString :: Applicativeable -> IO String
fString (Applicativeable f) = fmap show f

class ToApplicativeableOrApplicative r where
  toApplicative :: Bool -> r

instance ToApplicativeableOrApplicative Applicativeable where
  toApplicative :: Bool -> Applicativeable
  toApplicative True = Applicativeable $ pure 1
  toApplicative False = Applicativeable $ pure 2

instance Applicative f => ToApplicativeableOrApplicative (f Int) where
  toApplicative :: Bool -> f Int
  toApplicative True = pure 1
  toApplicative False = pure 2

------------------------------
-- This works (as expected) --
------------------------------

testFString :: Bool -> IO String
testFString = fString `compApplicativeable` toApplicative

compApplicativeable
  :: (Applicativeable -> IO String)
  -> (forall a. ToApplicativeableOrApplicative a => Bool -> a)
  -> Bool
  -> IO String
compApplicativeable = (.)

------------------------------------
-- This doesn't work (unexpected) --
------------------------------------

-- testFIntDoesNotWork1 :: Bool -> IO Int
-- testFIntDoesNotWork1 = fInt `compApplicativeDoesNotWork1` toApplicative

-- compApplicativeDoesNotWork1
--   :: ((forall g. Applicative g => g Int) -> IO Int)
--   -> (forall f. Applicative f => Bool -> f Int)
--   -> Bool
--   -> IO Int
-- compApplicativeDoesNotWork1 = (.)

------------------------------------
-- This doesn't work (unexpected) --
------------------------------------

-- testFIntDoesNotWork2 :: Bool -> IO Int
-- testFIntDoesNotWork2 = fInt `compApplicativeDoesNotWork2` toApplicative

-- compApplicativeDoesNotWork2
--   :: ((forall g. Applicative g => g Int) -> IO Int)
--   -> (forall a. ToApplicativeableOrApplicative a => Bool -> a)
--   -> Bool
--   -> IO Int
-- compApplicativeDoesNotWork2 = (.)

-----------------
-- This works? --
-----------------

testFIntWorks1 :: Bool -> IO Int
testFIntWorks1 = fInt `compApplicativeWorks1` toApplicative

compApplicativeWorks1
  :: ((forall g. Applicative g => g Int) -> IO Int)
  -> (forall f. Applicative f => Bool -> f Int)
  -> Bool
  -> IO Int
compApplicativeWorks1 g f b = g $ f b

-----------------
-- This works? --
-----------------

testFIntWorks2 :: Bool -> IO Int
testFIntWorks2 = fInt `compApplicativeWorks2` toApplicative

compApplicativeWorks2
  :: ((forall g. Applicative g => g Int) -> IO Int)
  -> (forall a. ToApplicativeableOrApplicative a => Bool -> a)
  -> Bool
  -> IO Int
compApplicativeWorks2 g f b = g $ f b
