module Homework2 where

import           Data.Char ()
import           Data.List

-- 1st problem
-- Checks whether the matrix has all rows of equal length.
isWellFormed :: [[Int]] -> Bool
isWellFormed []     = False
isWellFormed ([]:_) = False
isWellFormed [_]    = True
isWellFormed xss    = length (nub [length xs | xs <- xss]) == 1

-- Returns the dimensions of a n × m matrix as a pair (n,m).
size :: [[Int]] -> (Int, Int)
size xss
  | isWellFormed xss = (length xss, length $ head xss)
  | otherwise = error "Matrix is malformed"

-- Returns the element at the given position in matrix.
getElement :: [[Int]] -> Int -> Int -> Int
getElement xss x y
  | x < 0 || x >= rows || y < 0 || y >= columns = error "Index out of bounds"
  | otherwise = xss !! x !! y
  where
    rows = fst $ size xss
    columns = snd $ size xss

-- Returns the i-th row of a matrix.
getRow :: [[Int]] -> Int -> [Int]
getRow xss x
  | x < 0 || x >= rows = error "Index out of bounds"
  | otherwise = xss !! x
  where
     rows = fst $ size xss

-- Returns the i-th column of a matrix.
getCol :: [[Int]] -> Int -> [Int]
getCol xss y
  | y < 0 || y >= columns = error "Index out of bounds"
  | otherwise = [xs !! y | xs <- xss]
  where
    columns = snd $ size xss

-- Slices one big list in several lists of defined length.
splitAt' :: Int -> [Int] -> [[Int]]
splitAt' _ [] = []
splitAt' n xs
  | n <= 0 = error "Invalid split value"
  | (length xs `mod` n) /= 0 = error "Split value must be divisor of list length"
  | length sndPart > n = fstPart:splitAt' n sndPart
  | otherwise = [fstPart,sndPart]
  where
    fstPart = fst $ splitAt n xs
    sndPart = snd $ splitAt n xs

-- Adds two matrices.
addMatrices :: [[Int]] -> [[Int]] -> [[Int]]
addMatrices xss yss
  | size xss == size yss = splitAt' rowLength addedMatrices
  | otherwise = error "Matrices are not of equal size"
  where
    rowLength = snd $ size xss
    addedMatrices = zipWith (+) (concat xss) (concat yss)

-- Performs matrix transpose recursively.
transposeRec :: Int -> [[Int]] -> [[Int]]
transposeRec n xss
  | n < rowLength = [xs !! n | xs <- xss]: transposeRec (n+1) xss
  | otherwise = []
  where
    rowLength = length $ head xss

-- Returns a transposed version of the given matrix.
transpose' :: [[Int]] -> [[Int]]
transpose' xss
  | isWellFormed xss = transposeRec 0 xss
  | otherwise = error "Matrix is malformed"

-- Multiplies list elements on the coresponding indexes and sums them.
multLists :: [Int] -> [Int] -> Int
multLists [] _ = error "First list is empty"
multLists _ [] = error "Second list is empty"
multLists xs ys
  | length xs == length ys = sum $ zipWith (*) xs ys
  | otherwise = error "Lists are of different length"

-- Multiplies two matrices.
multMatrices :: [[Int]] -> [[Int]] -> [[Int]]
multMatrices xss yss
  | fstCol == sndRow = splitAt' sndCol [multLists xs ys | xs <- fstMat, ys <- sndMat]
  | otherwise = error "Incompatible matrix dimensions"
  where
    fstMat = xss
    sndMat = transpose' yss
    fstCol = snd $ size xss
    sndRow = fst $ size yss
    sndCol = snd $ size yss

-- 2nd problem
type Key = Int
type Value = String
type Entry = (Key, Value)
type Dictionary = [Entry]
type Frequency = [(Value, Int)]

-- Checks whether a given key exists in a dictionary.
exists :: Key -> Dictionary -> Bool
exists k dic = not $ null [key | (key,_) <- dic, key == k]

-- Returns the value associated with the given key.
get :: Dictionary -> Key -> Value
get dic k
  | not $ null search = head search
  | otherwise = "Key " ++ show k ++ " not found"
  where
    search = [value | (key,value) <- dic, key == k]

{-
  Returns the dictionary with the inserted entry. If the key is already
  present in the dictionary, function alters current value for that key.
-}
insert' :: Entry -> Dictionary -> Dictionary
insert' e dic
  | not $ exists key dic = e:dic
  | otherwise = [if fst t == key then (key, value) else t | t <- dic]
  where
    key = fst e
    value = snd e

-- Removes the entry for a given key.
delete' :: Key -> Dictionary -> Dictionary
delete' k dic = [t | t <- dic, fst t /= k]

-- Counts occurence of given value in the dictionary.
countOccurence :: Value -> Dictionary -> Int
countOccurence v dic = length [t | t <- dic, snd t == v]

-- Returns the number of appearances of every value in the dictionary.
freq :: Dictionary -> Frequency
freq []  = error "Dictionary is empty"
freq dic = nub [(v,countOccurence v dic) | (_, v) <- dic]

-- 3rd problem
-- Improvement upon requested function - takes divisor as argument.
largestMultiple' :: Int -> String -> Int
largestMultiple' 0 _ = error "Division by zero"
largestMultiple' _ [] = error "No value given"
largestMultiple' _ "0" = error "Zero given"
largestMultiple' _ ('-':_) = error "Negative value given"
largestMultiple' n xs
  | not $ null result = maximum result
  | otherwise = error "No such number"
  where
    result = [read x | x <- permutations xs, (read x `mod` n) == 0]

largestMultiple :: String -> Int
largestMultiple = largestMultiple' 30

-- 4th problem
{-
  This implementation receives no argument, and if evaluated will throw
  an exception. Function can be used in the general sense without evaluation.
  For example, evaluating:
  > length [undefined', undefined']
  will output '2'.
  Call doesn't crash because function itself is not evaluated, but merely
  used for 'lazy count.'
-}
undefined' :: a
undefined' = error "Can not be evaluated"

{-
  This implementation receives no argument, and returns function (itself).
  Function returned than also recieves no argument, resulting in infinite
  loop of recursive calls.
-}
undefined'' :: a
undefined'' = undefined''
