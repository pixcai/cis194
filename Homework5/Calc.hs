{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Calc where

import ExprT
import Parser
import qualified StackVM as VM

-- Exercise 1
eval :: ExprT -> Integer
eval exp = case exp of
             Lit v   -> v
             Add l r -> eval l + eval r
             Mul l r -> eval l * eval r

exercise1 = print $ eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr exp = fmap eval (parseExp Lit Add Mul exp)

exercise2 = do
  print $ evalStr "(2 + 3) * 4"
  print $ evalStr "(2 + 3) *"

-- Exercise 3
class Expr a where
  lit :: Integer -> a
  add, mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul
 
reify :: ExprT -> ExprT
reify = id

exercise3 = print $ eval $ reify (mul (add (lit 2) (lit 3)) (lit 4))

-- Exercise 4
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Ord, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 l) (Mod7 r) = lit (l + r)
  mul (Mod7 l) (Mod7 r) = lit (l * r)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(2 * 3) + 4"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7

exercise4 = do
  print testInteger
  print testBool
  print testMM
  print testSat

-- Exercise 5
instance Expr VM.Program where
  lit v   = VM.PushI v : []
  add l r = r ++ l ++ [VM.Add]
  mul l r = r ++ l ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul

exercise5 = print $ compile "(2 + 3) * 4"
