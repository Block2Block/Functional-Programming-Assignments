-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

-- Do NOT modify this file!

module Types where

data Number = One | Two | Three | Four deriving (Show,Eq)

data Tree a b = Leaf a | Node b [Tree a b] deriving (Show,Eq)

data Nat = Zero | Suc Nat

natToInt :: Nat -> Int
natToInt Zero    = 0
natToInt (Suc n) = 1 + natToInt n

absoluteValue :: Int -> Nat
absoluteValue = absoluteValueRec . abs
  where
    absoluteValueRec :: Int -> Nat
    absoluteValueRec 0 = Zero
    absoluteValueRec n = Suc (absoluteValue (pred n))

instance Show Nat where

  show n = "Nat " ++ show (natToInt n)
