module Exams where

import Data.Char

-- Problem 1
{-
  Returns the same sentence with all words capitalised except the ones
  from a given list.
-}
capitalise :: String -> [String] -> String
capitalise [] _ = []
capitalise xs xss = unwords $ map function (words xs)
  where
    function :: String -> String
    function [] = []
    function l@(y:ys) = if l `elem` xss then l else toUpper y : ys

-- Problem 2
{-
  Given a list of words, removes from the list all those which contain four or
  more vowels and those which have same letter appearing twice (or more) in a
  row. In addition, any word containing numbers will have the numbers removed
  before actual word filtering.
-}
weirdFilter :: [String] -> [String]
weirdFilter = filter vowelsFilter . filter lettersFilter .  map removeNumbers

-- Removes numbers from given String.
removeNumbers :: String -> String
removeNumbers = filter (not . isNumber)

-- Tells whether String has no characters appearing two or more times in a row.
lettersFilter :: String -> Bool
lettersFilter [] = True
lettersFilter [_] = True
lettersFilter (x:y:z)
  | x == y = False
  | otherwise = lettersFilter (y:z)

-- Tells whether String contains less than four vowels.
vowelsFilter :: String -> Bool
vowelsFilter xs = countVowels xs < 4

-- Counts number of vowels in given String.
countVowels :: String -> Int
countVowels = foldl (\acc x -> if toLower x `elem` vowels then acc + 1 else acc) 0

-- Vowels in English alphabet.
vowels :: String
vowels = ['a','e','i','o','u']

-- Problem 3
-- Calculates the greatest common divisor of two numbers - a and b.
gcd' :: Int -> Int -> Int
gcd' a b = gcd'' (abs a) (abs b)
  where
    gcd'' :: Int -> Int -> Int
    gcd'' x y
      | y > 0 = gcd' y (x `mod` y)
      | otherwise = x
