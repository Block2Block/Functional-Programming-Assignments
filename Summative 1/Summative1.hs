-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

-- You should copy this file to a new file called Summative1.hs and write your
-- solutions in that file.

-- A "fresh" template can be found at:
-- https://git.cs.bham.ac.uk/ahrensb/fp-learning-2020-2021/-/tree/master/Assignments/Summative1/Summative1-Template.hs

module Summative1 (catYrs , golfScorer , majority , luhn , bankCardNumbers , encrypt) where

import Data.Char

catYrs :: Integer -> Integer
catYrs x | x <= 0    = 0
         | x == 1    = 15
         | x == 2    = 24
         | otherwise = 24 + 4 * (x - 2)

golfScorer :: Integer -> Integer -> Integer
golfScorer x y | y == 1     = 5
               | x >= y + 2 = 4
               | elem y z =  ((x + 2) - y)
               | otherwise  = 0
              where z = [x-1..x+1]

majority :: (Bool,Bool,Bool) -> Bool
majority (True,x,y)  = x || y
majority (False,x,y) = x && y

luhnDouble :: Int -> Int
luhnDouble x | y > 9     = y - 9
             | otherwise = y
       where y = 2 * x

addUpLuhn :: [Int] -> Int -> Int
addUpLuhn x y | length x == 0           = y
              | (mod (length x) 2) == 1 = addUpLuhn (tail x) (y + head x)
              | otherwise               = addUpLuhn (tail x) (y + (luhnDouble (head x)))

luhn :: [Int] -> Bool
luhn x = mod (addUpLuhn x 0) 10 == 0

intToDigits :: Int -> [Int]
intToDigits x = map (read . (:[])) (show x)

digitsToInt :: [Int] -> Int
digitsToInt x = read(map intToDigit x)

makeFullLength :: [Int] -> Int -> [Int]
makeFullLength x y | length x == y = x
                   | otherwise = [0 | y <- [1..y-(length x)]] ++ x

bankCardNumbers :: Int -> [[Int]]
bankCardNumbers n = [makeFullLength (intToDigits x) n | x <- [0..digitsToInt [9 | y <- [1..n]]], luhn (makeFullLength (intToDigits x) n)]

let2int :: Char -> Int
let2int c = ord c - ord 'A'

int2let :: Int -> Char
int2let n = chr (ord 'A' + n)

shift n c | isUpper c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

encrypt :: String -> String -> String
encrypt x y = [shift (let2int (keyword !! z)) (y !! z) | z <- [0..((length y)-1)]]
    where keyword = take (length y) (cycle x)
