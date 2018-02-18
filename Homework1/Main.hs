module Main where
import Data.Char (digitToInt)
import Data.List (sum)

-- Exercise 1
toDigits, toDigitsRev :: Integer -> [Integer]
toDigits n    = if n > 0
                  then map (\c -> toInteger (digitToInt c)) (show n)
                  else []
toDigitsRev n = reverse (toDigits n)

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []       = []
doubleEveryOther (x:[])   = [x]
doubleEveryOther (x:s:xs) = [x, s * 2] ++ (doubleEveryOther xs)

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = sum (toDigits x) + (sumDigits xs)

-- Exercise 4
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigitsRev n)) `rem` 10 == 0

-- Exercise 5
type Peg  = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c | n == 0    = []
              | n == 1    = [(a, b)]
              | otherwise = (hanoi (n - 1) a c b) ++ [(a, b)] ++ (hanoi (n - 1) c b a)

-- Exercise 6
hanoi' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi' n a b c d | n == 0    = []
                 | n == 1    = [(a, b)]
                 | otherwise = []

main = do
  print $ validate 4012888888881881
  print $ validate 4012888888881882
  print $ hanoi 3 "a" "b" "c"
  print $ hanoi' 4 "a" "b" "c" "d"
