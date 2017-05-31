{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Lib where

import Control.Lens (Field1, Field2, Lens, Lens', ALens', _1, _2, cloneLens)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

incrementThrough :: (Int, Int) -> Lens' (Int, Int) Int -> IO (Int, Int)
incrementThrough a l = l (\b -> b + 1 <$ print b) a

incrementThrough' :: (Int, Int) -> ALens' (Int, Int) Int -> IO (Int, Int)
incrementThrough' a l = cloneLens l (\b -> b + 1 <$ print b) a

tuple :: (Int, Int)
tuple = (1, 2)

-- testIncrementThrough :: Bool -> IO (Int, Int)
-- testIncrementThrough = incrementThrough tuple . bFunc

testIncrementThrough' :: Bool -> IO (Int, Int)
testIncrementThrough' = incrementThrough' tuple . bFunc

bFunc :: (Field1 s t a b, Field2 s t a b, s ~ t, a ~ b) => Bool -> Lens' s a
bFunc b = if b then _1 else _2
