{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Lib where

import Control.Lens (Field1, Field2, Lens, Lens', ALens', _1, _2, cloneLens)
import Control.Lens.Internal.Context

someFunc :: IO ()
someFunc = putStrLn "someFunc"

tuple :: (Int, Int)
tuple = (1, 2)

---------------------------------------------------------------------

incrementThroughExpandLens :: (forall f. Functor f => (Int -> f Int) -> (Int, Int) -> f (Int, Int)) -> IO (Int, Int)
incrementThroughExpandLens l = l (\b -> b + 1 <$ print b) tuple

incrementThroughExpandLens' :: ALens' (Int, Int) Int -> IO (Int, Int)
incrementThroughExpandLens' l = cloneLens l (\b -> b + 1 <$ print b) tuple

testIncrementThrough :: Bool -> IO (Int, Int)
testIncrementThrough = incrementThroughExpandLens `comp` bFuncExpandLens

testIncrementThrough' :: Bool -> IO (Int, Int)
testIncrementThrough' = incrementThroughExpandLens' `comp'` bFuncExpandLens

bFuncExpandLens
  :: forall f.
     Functor f
  => Bool -> (Int -> f Int) -> (Int, Int) -> f (Int, Int)
bFuncExpandLens b = if b then _1 else _2

comp
  :: ((forall g. Functor g => (Int -> g Int) -> (Int, Int) -> g (Int, Int)) -> IO (Int, Int))
  -> (forall f. Functor f => Bool -> (Int -> f Int) -> (Int, Int) -> f (Int, Int))
  -> Bool
  -> IO (Int, Int)
comp = (.)

comp'
  :: (ALens' (Int, Int) Int -> IO (Int, Int))
  -- -> (Bool -> (Int -> (Pretext (->) Int Int) Int) -> (Int, Int) -> (Pretext (->) Int Int) (Int, Int))
  -> (forall f. Functor f => Bool -> (Int -> f Int) -> (Int, Int) -> f (Int, Int))
  -> Bool
  -> IO (Int, Int)
comp' = (.)
