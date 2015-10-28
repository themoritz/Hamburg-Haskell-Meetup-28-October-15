{-# LANGUAGE ExistentialQuantification #-}
module Reducer where

import Data.Monoid

-- | A naive solution to summing up an average.
avg0 xs = fromIntegral (sum xs) / fromIntegral (length xs)

-- | Second try:
--   This time through threading a tuple as accumulator.
avg1 xs = fromIntegral s / fromIntegral n
  where
    (s, n) = foldl (\(s, i) x -> (s + x, i + 1) ) (0, 0) xs


-- | Third try:
--   Finding an abstraction for accumulations.
data Reducer a b = forall m. Monoid m => R (a -> m) (m -> b)

instance Functor (Reducer a) where
  fmap f (R h k) = R h (f . k)

instance Applicative (Reducer a) where
  -- pure :: b -> Reducer a b
  pure x = R (\_ -> ()) (\_ -> x)

  -- (<*>) :: Reducer a (b -> c) -> Reducer a b -> Reducer a c
  R f g <*> R h k = R (\x -> (f x, h x)) (\(fx, hx) -> (g fx) (k hx))

sumr :: Num a => Reducer a a
sumr = R Sum getSum

lengthr :: Num b => Reducer a b
lengthr = R (\_ -> Sum 1) getSum

avg2 :: Fractional a => Reducer a a
avg2 = (/) <$> sumr <*> lengthr

reduce :: Foldable t => Reducer a b -> t a -> b
reduce (R f g) = g . foldMap f
