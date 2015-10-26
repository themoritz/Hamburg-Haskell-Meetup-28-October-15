{-# LANGUAGE ExistentialQuantification #-}

module Exists where

import Prelude hiding (head, take)

data IntStream
  = forall a. MkIntStream a (a -> (Int, a))

naturals :: IntStream
naturals = MkIntStream 0 (\n -> (n, n+1))

naturalsList :: IntStream
naturalsList = MkIntStream [] (\l -> (length l, ():l))

head :: IntStream -> Int
head (MkIntStream new get) = fst $ get new

take :: Int -> IntStream -> [Int]
take x (MkIntStream new get) = go x new
  where go x' stream = if x' <= 0
          then []
          else let (result, newStream) = get stream
               in result:(go (x'-1) newStream)

test :: IO ()
test = do
  print $ head   naturals
  print $ take 5 naturals
  print $ take 5 naturalsList
