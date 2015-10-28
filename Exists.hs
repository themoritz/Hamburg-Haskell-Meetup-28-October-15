{-# LANGUAGE ExistentialQuantification #-}

module Exists where

import Prelude hiding (head, take)

data IntStream
  = forall a. MkIntStream
                a                -- seed
                (a -> (Int, a))  -- get

naturals :: IntStream
naturals = MkIntStream 0 (\n -> (n, n+1))

naturalsList :: IntStream
naturalsList = MkIntStream [] (\l -> (length l, ():l))

head :: IntStream -> Int
head (MkIntStream seed get) = fst $ get seed

take :: Int -> IntStream -> [Int]
take x (MkIntStream seed get) = go x seed
  where go x' state = if x' <= 0
          then []
          else let (result, newState) = get state
               in result:(go (x'-1) newState)

test :: IO ()
test = do
  print $ head   naturals
  print $ take 5 naturals
  print $ take 5 naturalsList
