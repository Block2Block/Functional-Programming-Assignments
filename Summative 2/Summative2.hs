-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Summative2 (encodeWord , encodeWords , encodeText ,
                   decodeText ,
                   decodeTextWithTree ,
                   ramify ,
                   tabulate ,
                   tree ,
                   simplify , cf , infFrac , eseq , eApproximate) where

import Types
import Data.List.Split
import Data.List
---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

{- Question 1 -}

shortSpace :: [Code] -> [Code]
shortSpace [] = []
shortSpace [x] = [x]
shortSpace (x:xs) = x:shortGap:(shortSpace xs)

encodeWord :: Table -> String -> Code
encodeWord _ [] = []
encodeWord [] _ = []
encodeWord t s = concat (shortSpace ([snd x | y <- s, x <- [z | z <- t, (fst z) == y]]))

mediumSpace :: [Code] -> [Code]
mediumSpace [] = []
mediumSpace [x] = [x]
mediumSpace (x:xs) = x:mediumGap:(mediumSpace xs)

encodeWords :: Table -> [String] -> Code
encodeWords t s = concat (mediumSpace [encodeWord t y | y <- s])


encodeText :: Table -> String -> Code
encodeText t s = encodeWords t (splitOn " " s)

{- Question 2 -}

decodeWord :: Table -> Code -> String
decodeWord [] _ = []
decodeWord _ [] = []
decodeWord t c = [fst x | l <- (splitOn [Silence, Silence, Silence] c), x <- [z | z <- t, (snd z) == (l ++ [Silence]) || (snd z) == l]]

addSpace :: [String] -> [String]
addSpace [] = []
addSpace [x] = [x]
addSpace (x:xs) = x:" ":(addSpace xs)

decodeText :: Table -> Code -> String
decodeText t c = concat (addSpace [decodeWord t x | x <- (splitOn [Silence, Silence, Silence, Silence, Silence, Silence, Silence] c)])

{- Question 3 -}

decodeCharWithTree :: Tree -> [Code] -> Char
decodeCharWithTree Empty _ = ' '
decodeCharWithTree (Branch (Just a) b c) [] = a
decodeCharWithTree (Branch (Nothing) b c) [] = ' '
decodeCharWithTree (Branch _ b c) (x:[[]]) = if (x ++ [Silence]) == dit then decodeCharWithTree b [] else decodeCharWithTree c []
decodeCharWithTree (Branch _ b c) (x:xs) = if (x ++ [Silence]) == dit then decodeCharWithTree b xs else decodeCharWithTree c xs

decodeWordWithTree :: Tree -> Code -> String
decodeWordWithTree t c = [decodeCharWithTree t [x |x <- (splitOn [Silence] z)] | z <- (splitOn [Silence, Silence, Silence] c)]

decodeTextWithTree :: Tree -> Code -> String
decodeTextWithTree t c = concat (addSpace [decodeWordWithTree t x | x <- (splitOn [Silence, Silence, Silence, Silence, Silence, Silence, Silence] c)])


{- Question 4 -}

treeInsertSplit :: Tree -> Char -> [Code] -> Tree
treeInsertSplit Empty ch [] = Branch (Just ch) Empty Empty
treeInsertSplit Empty ch (x:xs) = Branch Nothing (if (x ++ [Silence] == dit || x == dit) then treeInsertSplit Empty ch xs else Empty) (if (x ++ [Silence] == dah || x == dah) then treeInsertSplit Empty ch xs else Empty)
treeInsertSplit (Branch _ b c) ch [] = Branch (Just ch) b c
treeInsertSplit (Branch a b c) ch (x:xs) = Branch a (if (x ++ [Silence] == dit || x == dit) then treeInsertSplit b ch xs else b) (if (x ++ [Silence] == dah || x == dah) then treeInsertSplit c ch xs else c)

treeInsert :: Tree -> (Char, Code) -> Tree
treeInsert t (ch, co) = treeInsertSplit t ch (init (splitOn [Silence] co))

ramify :: Table -> Tree
ramify [] = Empty
ramify t = foldl (treeInsert) Empty t

{- Question 5 -}
tabulate :: Tree -> Table
tabulate Empty = []
tabulate t = tabulateRecursively t []

tabulateRecursively :: Tree -> Code -> Table
tabulateRecursively Empty _ = []
tabulateRecursively (Branch (Nothing) a b) x  = (tabulateRecursively a (x ++ dit)) ++ (tabulateRecursively b (x ++ dah))
tabulateRecursively (Branch (Just a) b c) x = [(a,x)] ++ (tabulateRecursively b (x ++ dit)) ++ (tabulateRecursively c (x ++ dah))


{- Question 6 -}
brackets :: Bracket -> String
brackets (Round ts) = "(" ++ concat [brackets t | t <- ts] ++ ")"
brackets (Curly ts) = "{" ++ concat [brackets t | t <- ts] ++ "}"

tree :: String -> Maybe Bracket
tree [] = Nothing
{- Having a single bracket/character in the list ever means that it is impossible for the string to be balanced -}
tree (x:[]) = Nothing

tree "{}" = Just (Curly [])
tree "()" = Just (Round [])

tree (x:xs) = if (x == '(')
                then (if ((last xs) == ')')
                        then (case (treeRecursively (init xs) "") of
                                    Nothing -> Nothing
                                    Just a -> Just (Round a))
                        else (Nothing))
                else (if (x == '{')
                        then (if ((last xs) == '}')
                                then (case (treeRecursively (init xs) "") of
                                    Nothing -> Nothing
                                    Just a -> Just (Curly a))
                                else Nothing)
                        else Nothing)

{- Argument 1 - the string to tree itself
   Argument 2 - A string which will act as a stack so I know when there are unclosed/extra/wrong brackets. -}
treeRecursively :: String -> String -> Maybe [Bracket]

{- Having a single bracket/character in the list and not having anything in the stack (and vice versa) means that it is impossible for the string to be balanced as there are either unescaped/extra closing brackets. -}
treeRecursively (x:[]) [] = Nothing
treeRecursively [] (x:xs) = Nothing


treeRecursively [] _ = Just []

{- Catch the eventuality that we reach the deepest part of the stack -}
treeRecursively (x:xs) s = if (x == '(')
                               then (if ((head xs) == ')')
                                       then (case (treeRecursively (tail xs) s) of
                                               Nothing -> Nothing
                                               Just a -> Just ([Round []] ++ a))
                                       else (if ((head xs) == '}')
                                               then (Nothing)
                                               else (case (treeRecursively (xs) (s ++ "(")) of
                                                   Nothing -> Nothing
                                                   Just a -> (case (treeRecursively (nextSection xs s) s) of
                                                               Nothing -> Nothing
                                                               Just b -> Just ([Round a] ++ b)))))
                               else (if (x == ')')
                                   then (if (length s == 0)
                                           then (Nothing)
                                           else (case (last s) of
                                                   '(' -> Just []
                                                   a -> Nothing))
                                   else (if (x == '{')
                                           then (if ((head xs) == '}')
                                                   then (case (treeRecursively (tail xs) s) of
                                                           Nothing -> Nothing
                                                           Just a -> Just ([Curly []] ++ a))
                                                   else (if ((head xs) == ')')
                                                           then (Nothing)
                                                           else (case (treeRecursively (xs) (s ++ "{")) of
                                                               Nothing -> Nothing
                                                               Just a -> (case (treeRecursively (nextSection xs s) s) of
                                                                   Nothing -> Nothing
                                                                   Just b -> Just ([Curly a] ++ b)))))
                                           else (if (x == '}')
                                                   then (if (length s == 0)
                                                           then (Nothing)
                                                           else (case (last s) of
                                                                   '{' -> Just []
                                                                   a -> Nothing))
                                                   else (Nothing))))

nextSection :: String -> String -> String
nextSection [] _ = []
nextSection x s = case (treeRecursively x s) of
                    Nothing -> nextSection (tail x) s
                    a -> x

isWellBracketed :: String -> Bool
isWellBracketed xs = case tree xs of
                      Nothing -> False
                      Just _  -> True

{- Question 7 -}
simplify :: Expr -> Frac
simplify = undefined

cf :: Integer -> [Integer] -> Expr
cf = undefined

infFrac :: Integer -> [Integer] -> Int -> Expr
infFrac = undefined

eseq :: [Integer]
eseq = undefined

eApproximate :: Int -> Frac
eApproximate = undefined

compute :: Frac -> Double
compute (m :/: n) = (fromIntegral m) / (fromIntegral n)
