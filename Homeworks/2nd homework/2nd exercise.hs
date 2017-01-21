module Exercise2 where

import           Data.Char
import           Data.List

-- LECTURE 4
-- Exercise 1.1
headhunter :: [[a]] -> a
headhunter []              = error "Empty list"
headhunter ((x:_):_)       = x
headhunter ([]:(x:_):_)    = x
headhunter ([]:[]:(x:_):_) = x
headhunter _               = error "Invalid input"

-- Checks whether the matrix has all rows of equal length.
isWellFormed :: [[a]] -> Bool
isWellFormed []     = False
isWellFormed ([]:_) = False
isWellFormed [_]    = True
isWellFormed xss    = length (nub [length xs | xs <- xss]) == 1

-- Exercise 1.2
firstColumn :: [[a]] -> [a]
firstColumn xss
  | isWellFormed xss = [x | (x:_) <- xss]
  | otherwise = error "Matrix is not valid"

-- Exercise 1.3
shoutOutLoud :: String -> String
shoutOutLoud xss =
  unwords [replicate 3 h ++ t | (h:t) <- words xss]

-- Exercise 2.1
-- Capitalizes and adds n trailing spaces to the end of the string.
padAndCapitalize :: Int -> String -> String
padAndCapitalize n []     = replicate n ' '
padAndCapitalize n (l:ls) = toUpper l:ls ++ replicate n ' '

pad :: String -> String -> (String, String)
pad [] [] = ([],[])
pad xs ys
  | l1 < l2 = (padAndCapitalize diff xs, toUpper (head ys): tail ys)
  | otherwise = (toUpper (head xs): tail xs, padAndCapitalize diff ys)
  where
    l1 = length xs
    l2 = length ys
    diff = abs (l1 - l2)

-- Exercise 2.2
-- Function taken from lecture.
median :: (Integral a, Fractional b) => [a] -> b
median [] = error "median: Empty list"
median xs
  | odd l     = realToFrac $ ys !! h
  | otherwise = realToFrac (ys !! h + ys !! (h-1)) / 2
  where l  = length xs
        h  = l `div` 2
        ys = sort xs

quartiles :: [Int] -> (Double,Double,Double)
quartiles [] = error "Empty list"
quartiles xs
  | l >= 2 = (median fstHalf, median xs', median sndHalf)
  | otherwise = error "List needs to be of size 2 or greater"
  where
    l = length xs
    xs' = sort xs
    fstHalf = take (l `div` 2) xs'
    sndHalf = if even l then drop (l `div` 2) xs' else drop (l `div` 2 + 1) xs'

-- Exercise 3.1
pad' :: String -> String -> (String, String)
pad' [] [] = ([],[])
pad' xs ys =
  let
  l1' = length xs
  l2' = length ys
  diff = abs (l1' - l2')
    in  if l1' < l2'
        then (padAndCapitalize diff xs, toUpper (head ys): tail ys)
        else  (toUpper (head xs): tail xs, padAndCapitalize diff ys)

-- Exercise 3.2
-- Function from lecture rewritten with 'let'.
median' :: (Integral a, Fractional b) => [a] -> b
median' [] = error "median: Empty list"
median' xs =
  let
    l  = length xs
    h  = l `div` 2
    ys' = sort xs
    in  if odd l
        then realToFrac $ ys' !! h
        else realToFrac (ys' !! h + ys' !! (h-1)) / 2

quartiles' :: [Int] -> (Double,Double,Double)
quartiles' [] = error "Empty list"
quartiles' xs =
  let
    l = length xs
    xs' = sort xs
    fstHalf' = take (l `div` 2) xs'
    sndHalf' = if even l then drop (l `div` 2) xs' else drop (l `div` 2 + 1) xs'
    in  if l >= 2
        then (median fstHalf', median xs', median sndHalf')
        else error "List needs to be of size 2 or greater"

-- Exercise 4.1
analyzer :: (Show c) => (Int,Int) -> [c] -> String
analyzer (a,b) xs =
  "The pair" ++ case (a,b) of
    (1,1) -> " contains two ones "
    (1,_) -> " contains one one "
    (_,1) -> " contains one one "
    (_,_) -> " does not contain a single one "
  ++  if length xs >= 2
      then "and the second element of the list is " ++ show (xs !! 1)
      else "and the list does not have second element"
