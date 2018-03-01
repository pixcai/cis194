module Golf where

import Data.Bool (bool)
import Data.List (transpose, group, sort)

-- Exercise 1
skips :: [a] -> [[a]]
skips xs = 
  map f [1..n]
  where
    n   = length xs
    f x = map (xs !!) (filter (< n) (take n (iterate (+ x) (x - 1))))

-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs) = bool [] [y] (x < y && y > z) ++ localMaxima (y:z:xs)
localMaxima _          = []

-- Exercise 3
histogram :: [Integer] -> String
histogram xs =
  foldr ((++) . line) "==========\n0123456789\n" ys
  where
    ys      = (reverse . transpose . group . sort) xs
    line zs = map ((bool ' ' '*') . (`elem` zs)) [0..9] ++ "\n"

main = do
  putStr "\n------ Exercise 1 ------\n"
  print $ skips "ABCD"
  print $ skips "hello!"
  print $ skips [1]
  print $ skips [True, False]
  print $ skips ([]::[Int])
  putStr "\n------ Exercise 2 ------\n"
  print $ localMaxima [2,9,5,6,1]
  print $ localMaxima [2,3,4,1,5]
  print $ localMaxima [1,2,3,4,5]
  putStr "\n------ Exercise 3 ------\n"
  putStr $ histogram [1,1,1,5]
  putStr $ histogram [1,4,5,4,6,6,3,4,2,4,9]
