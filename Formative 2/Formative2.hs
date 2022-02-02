-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

-- You should copy this file to a new file called Formative2.hs and write your
-- solutions in that file.

-- A "fresh" template can be found at:
-- https://git.cs.bham.ac.uk/ahrensb/fp-learning-2020-2021/-/tree/master/Assignments/Formative2/Formative2-Template.hs

module Formative2 (toTuple , mapList , coinChange , leavesAndNodes , treeMap , treeFold' , treeFold , natrec) where

import Types

toTuple :: (c -> a) -> (c -> b) -> c -> (a,b)
toTuple x y z = (x z, y z)

mapList :: [a -> b] -> [a] -> [b]
mapList [] _ = []
mapList a b = [x y | x <- a, y <- b]

calculateRunningTotal :: [Integer] -> [Integer] -> Integer
calculateRunningTotal a b = sum [x * y | z<-[0..(length b)-1], x <- [a !! z], y <- [b !! z]]

coinChange :: Integer -> [Integer] -> [Integer]
coinChange x y | length y > 0 = (div (x - calculateRunningTotal z (tail y)) (head y)) : z
               | otherwise    = []
            where z = coinChange x (tail y)

leavesAndNodes :: Tree a b -> ([a],[b])
leavesAndNodes (Leaf a)     = ([a],[])
leavesAndNodes (Node b x) = ([y | w <- z, y <- fst w], [b] ++ [y | w <- z, y <- snd w])
                where z = [leavesAndNodes y | y <- x]

treeMap :: (a -> c) -> (b -> d) -> Tree a b -> Tree c d
treeMap f g (Leaf a)   = Leaf (f a)
treeMap f g (Node b x) = Node (g b) [treeMap f g y | y <- x]

treeFold' :: (a -> c) -> (b -> [c] -> c) -> Tree a b -> c -- *stretcher*
treeFold' f g (Leaf a)   = f a
treeFold' f g (Node b x) = g b [treeFold' f g y | y <- x]

treeFold :: (a -> c -> c) -> (b -> [c] -> c) -> c -> Tree a b -> c -- *stretcher*
treeFold = undefined


natrec :: (Nat -> a -> a) -> a -> Nat -> a -- *stretcher*
natrec = undefined
