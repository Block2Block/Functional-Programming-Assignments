-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Summative3 (experiments ,
                   gameAnnotated ,
                   game ,
                   odds ,
                   oneOf ,
                   noun , verb , pronoun , properNoun , determiner , preposition ,
                   nominal ,
                   np ,
                   vp ,
                   pp ,
                   sent) where

import Types
import Parsing
import Data.Char

---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------

import Data.List
import Data.List.Split (splitOn)

{- Monads and a game of chance -}

-- An example you can use for testing
headsOrTails :: ChanceMonad m => m Outcome
headsOrTails = do
  c <- toss
  if c == H then return Win else return Lose

{- Exercise 1.1 -}
experiments :: ChanceMonad m => m a -> Integer -> m [a]
experiments x 0 = return []
experiments x y = do
                c <- x
                d <- experiments x (y-1)
                return (c:d)

{- Exercise 1.2 -}
gameAnnotated :: ChanceMonad m => m ([Coin],Die,Outcome)
gameAnnotated = do
                c <- experiments toss 6
                d <- roll
                return (c, d, (isWin c d))

isWin :: [Coin] -> Die -> Outcome
isWin x y | y == D1 && 2 > z = Win
          | y == D2 && 3 > z = Win
          | y == D3 && 4 > z = Win
          | y == D4 && 5 > z = Win
          | y == D5 && 6 > z = Win
          | y == D6 = Win
          | otherwise = Lose
      where
          z = sum [1 | z <- x, z == H]

{- Exercise 1.3 -}
game :: ChanceMonad m => m Outcome
game = do
       c <- experiments toss 6
       d <- roll
       return (isWin c d)

{- Exercise 1.4 -}
odds :: [Outcome] -> Float
odds [] = 0.0
odds x = (fromIntegral (sum [1 | y <- x, y == Win])) / (fromIntegral (length x))

{- Parsing English -}
parseTest :: Parser Tree -> String -> Maybe Tree
parseTest p s = case (parse p s) of
                  [(t,"")] -> Just t
                  _        -> Nothing

{- Exercise 2.1 -}
oneOf :: [String] -> Parser String
oneOf x = P (\inp -> [(y, (drop (length y) inp)) | y <- x, isPrefixOf y inp])

{- Exercise 2.2 -}
noun :: Parser Tree
noun = P (\inp -> case (parse (oneOf nouns) inp) of
                    [] -> []
                    ((x, y):xs) -> [(Leaf Noun x, y)])
verb :: Parser Tree
verb = P (\inp -> case (parse (oneOf verbs) inp) of
                    [] -> []
                    ((x, y):xs) -> [(Leaf Verb x, y)])
pronoun :: Parser Tree
pronoun = P (\inp -> case (parse (oneOf pronouns) inp) of
                    [] -> []
                    ((x, y):xs) -> [(Leaf Pronoun x, y)])
properNoun :: Parser Tree
properNoun = P (\inp -> case (parse (oneOf properNouns) inp) of
                    [] -> []
                    ((x, y):xs) -> [(Leaf ProperNoun x, y)])
determiner :: Parser Tree
determiner = P (\inp -> case (parse (oneOf determiners) inp) of
                    [] -> []
                    ((x, y):xs) -> [(Leaf Determiner x, y)])
preposition :: Parser Tree
preposition = P (\inp -> case (parse (oneOf prepositions) inp) of
                    [] -> []
                    ((x, y):xs) -> [(Leaf Preposition x, y)])

{- Exercise 2.3 -}
nominal :: Parser Tree
nominal = do
            x <- noun
            space'
            y <- nominal
            return (Branch Nominal [x, y])
          <|> do
            x <- noun
            return (Branch Nominal [x])

space' :: Parser ()
space' = do some (sat isSpace)
            return ()

{- Exercise 2.4 -}
np :: Parser Tree
np = do
        x <- pronoun
        return (Branch NP [x])
     <|> do
        x <- properNoun
        return (Branch NP [x])
     <|> do
        x <- determiner
        space'
        y <- nominal
        return (Branch NP [x, y])

{- Exercise 2.5 -}
vp :: Parser Tree
vp = do
        x <- verb
        space'
        y <- np
        space'
        z <- pp
        return (Branch VP [x, y, z])
     <|> do
        x <- verb
        space'
        y <- np
        return (Branch VP [x, y])
     <|> do
        x <- verb
        space'
        y <- pp
        return (Branch VP [x, y])
     <|> do
        x <- verb
        return (Branch VP [x])

{- Exercise 2.6 -}
pp :: Parser Tree
pp = do
        x <- preposition
        space'
        y <- np
        return (Branch PP [x, y])

{- Exercise 2.7 -}
sent :: Parser Tree
sent = do
        x <- np
        space'
        y <- vp
        return (Branch Sentence [x, y])

parseSentence :: String -> Maybe Tree
parseSentence = parseTest sent
