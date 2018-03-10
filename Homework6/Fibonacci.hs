module Fibonacci where

import Data.List

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

exercise1 = do
  print $ fib 10
  print $ take 10 fibs1

-- Exercise 2
fibs2 :: [Integer]
fibs2 = foldr (\a b -> fib a : b) [] [0..]

exercise2 = print $ take 10 fibs2

-- Exercise 3
data Stream a = Cons [a]

instance Show a => Show (Stream a) where
  show (Cons xs) = show (take 10 xs)

streamToList :: Stream a -> [a]
streamToList (Cons xs) = xs

exercise3 = print $ take 10 (streamToList (Cons [0..]))

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons (repeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons xs) = Cons (map f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons (iterate f x)

exercise4 = do
  print $ streamRepeat 1
  print $ streamMap (* 2) (Cons [0..])
  print $ streamFromSeed (* 2) 1

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

-- ruler :: Stream Integer

exercise5 = do
  print $ nats
