-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

---------------------------------------------------------------------------------
-------------------------- DO **NOT** MODIFY THIS FILE --------------------------
---------------------------------------------------------------------------------

module Types where

import System.Random

{- Questions 1 - 3 -}
data Expr = Val Int | Add Expr Expr

{- Question 4 -}
data MyMaybe a = MyNothing | MyJust a

data MyList a = MyEmpty | MyCons a (MyList a)

{- Question 5 -}
-- Valid buttons are ['0'..'9']++['*','#']
type Button = Char
-- Valid presses are [1..]
type Presses = Int
-- Valid text consists of
-- ['A'..'Z']++['a'...'z']++['0'..'9']++['.',',',' ']
type Text = String

{- Question 7 -}
data Rand a = Generator (StdGen -> (a , StdGen))

instance Monad Rand where
 return x = Generator (\g -> (x,g))
 Generator h >>= f = Generator (\g -> let (x, g') = h g
                                          (Generator h') = f x
                                      in h' g')

instance Functor Rand where
   fmap f xm = xm >>= return . f

instance Applicative Rand where
   pure = return
   fm <*> xm = fm >>= \f -> xm >>= return . f

runRand :: Int -> Rand a -> a
runRand seed (Generator h) = fst (h (mkStdGen seed))

randInt :: Rand Int
randInt = Generator random

randIntR :: (Int, Int) -> Rand Int
randIntR (lower, upper) = Generator (randomR (lower, upper))
