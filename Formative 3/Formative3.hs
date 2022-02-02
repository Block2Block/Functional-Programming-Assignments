-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Formative3 (folde , eval , size , exprToString , phoneToString , stringToPhone , fingerTaps , qsortm , rqsort) where

import Types

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 1 -}
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x) = (f x)
folde f g (Add x y) = g (folde f g x) (folde f g y)


{- Question 2a -}
eval :: Expr -> Int
eval x = folde (*1) (\y z -> (y+z)) x

{- Question 2b -}
size :: Expr -> Int
size x = folde (\w -> 1) (\y z -> (y+z)) x

{- Question 3 -}
exprToString :: Expr -> String
exprToString x = folde (show) (\y z -> y ++ " + " ++ z) x

{- Question 4a -}
instance Eq a => Eq (MyMaybe a) where
  (==) = undefined

{- Question 4b -}
instance Eq a => Eq (MyList a) where
  (==) = undefined

{- Question 4c -}
instance Ord a => Ord (MyMaybe a) where
  (<=) = undefined

{- Question 5a -}
phoneToString :: [(Button, Presses)] -> Text
phoneToString = undefined

{- Question 5b -}
stringToPhone :: Text -> [(Button, Presses)]
stringToPhone = undefined

{- Question 5c -}
fingerTaps :: Text -> Presses
fingerTaps = undefined

{- Question 6 -}
qsortm :: (Ord a , Monad m) => [a] -> m [a]
qsortm = undefined

-- Pre-defined helper for you to use if you want
getPivot :: [a] -> Int -> (a, [a])
getPivot []     _ = undefined
getPivot (x:xs) 0 = (x,xs)
getPivot (x:xs) n = let (p,ys) = getPivot xs (n-1) in (p, x:ys)

{- Question 7 -}
rqsort :: Ord a => [a] -> Rand [a]
rqsort = undefined

-- For testing
rqsort' :: Ord a => [a] -> [a]
rqsort' xs = runRand seed (rqsort xs)
             where seed = 42 -- say
