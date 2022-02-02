-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}


-- a "fresh" template can be found at
-- https://git.cs.bham.ac.uk/ahrensb/fp-learning-2020-2021/-/tree/master/Assignments/Formative1/Formative1-Template.hs


module Formative1 (capitalized , isLeapYear , gasUsage , luhnDouble , luhn) where

import Types

capitalized :: [Char] -> Bool
capitalized [] = False
capitalized (x:xs) = elem x ['A'..'Z']

isLeapYear :: Int -> Bool
isLeapYear y | mod y 400 == 0 || (mod y 4 == 0 && mod y 100 /= 0) = True
             | otherwise                                          = False


gasUsage :: (Fractional a, Ord a) => a -> Classification
gasUsage g | g < 3           = Low
           | 3 <= g && g < 5 = Medium
           | 5 <= g && g < 7 = High
           | otherwise       = SuperHigh

luhnDouble :: Int -> Int
luhnDouble x | y > 9     = y - 9
             | otherwise = y
       where y = 2 * x


luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (mod (d + (luhnDouble c) + b + (luhnDouble a)) 10) == 0


