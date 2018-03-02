module Main where

import Data.Bool (bool)
import Data.List ((\\))

-- Exercise 1
fun1' :: [Integer] -> Integer
fun1' = product . map ((-) 2) . filter even

fun2' :: Integer -> Integer
fun2' =
  sum . filter even . takeWhile ((/=) 1) . iterate p
  where
    p n = bool (3 * n + 1) (n `div` 2) (even n)

exercise1 = do
  putStr "\n------ Exercise 1 ------\n"
  print $ fun1' []
  print $ fun1' [3,5,6,1,8]
  print $ fun2' 1
  print $ fun2' 4
  print $ fun2' 5

-- Exercise 2
data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree =
  foldr p Leaf
  where
    p v tree             =
      case tree of
        Node n Leaf v' r -> Node (n + 1) (p v Leaf) v' r
        Node n l v' Leaf -> Node n l v' (p v Leaf)
        Node _ l v' r    -> 
          bool (Node n u v' r) (Node n l v' u) t
          where
            t = level l >= level r
            u = p v (bool l r t)
            n = level u + 1
        _                -> Node 0 Leaf v Leaf
    level (Node n _ _ _) = n

exercise2 = do
  putStr "\n------ Exercise 2 ------\n"
  print $ foldTree "ABCDEFGHIJ"

-- Exercise 3
xor :: [Bool] -> Bool
xor = foldr (\a b -> bool b (not b) a) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a b -> f a : b) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

exercise3 = do
  putStr "\n------ Exercise 3 ------\n"
  print $ xor []
  print $ xor [False, True]
  print $ xor [True, True]
  print $ map' (+ 1) []
  print $ map' (+ 1) [1..9]
  print $ myFoldl (+) 0 []
  print $ myFoldl (\a b -> b : a) [] [1..9]

-- Exercise 4
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
  (takeWhile (< (2 * n + 2)) . map ((+ 1) . (2 *))) xs
  where
    fp (i, j) k = let x = i + j + 2 * i * j in bool k (x:k) (i <= j && x <= n)
    xs          = let ys = [1..n] in ys \\ (foldr fp [] (cartProd ys ys))

exercise4 = do
  putStr "\n------ Exercise 4 ------\n"
  print $ sieveSundaram 100

main = do
 exercise1
 exercise2
 exercise3
 exercise4
