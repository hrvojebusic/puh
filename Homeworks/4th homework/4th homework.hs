module Homework4 where

import           Data.Char

-- Problem 1

-- Inserts the list xs in between the lists in yss and concatenates the result.
intercalate' :: [a] -> [[a]] -> [a]
intercalate' as bss = concat $ intercalate'' as bss
  where
    intercalate'' :: [a] -> [[a]] -> [[a]]
    intercalate'' _ []        = []
    intercalate'' [] yss      = yss
    intercalate'' _ [ys]      = [ys]
    intercalate'' xs (ys:yss) = ys : xs : intercalate'' xs yss

-- Problem 2

{-
  Splits up a list xs into sublist of length n. If the length of xs is not a
  multiple of n, the last sublist will be shorter than n.
-}
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs
  | n < 0 = error "Negative chunk length"
  | n == 0 = []
  | otherwise = take n xs : chunk n (drop n xs)

{-
  Splits up a list ys into sublists of lengths given in a list of indices xs.
  If the lengths in xs do not add up to the length of ys, the remaining part
  of ys will remain unchunked.
-}
chunkBy :: [Int] -> [a] -> [[a]]
chunkBy [] _ = []
chunkBy _ [] = []
chunkBy (x:xs) ys
  | x < 0 = error "Negative chunk length"
  | x == 0 = chunkBy xs ys
  | otherwise = take x ys : chunkBy xs (drop x ys)

{-
  Splits up a list xs into n sublists of equal length. If the length of xs
  is not divisible by n, it puts the remainder into the last sublist.
-}
chunkInto :: Int -> [a] -> [[a]]
chunkInto _ [] = []
chunkInto 0 _ = []
chunkInto n xs
  | n < 0 = error "Negative chunk length"
  | otherwise = chunkInto' n c xs
  where
    c = length xs `div` n -- length of chunks -> 'regular' chunks
    chunkInto' :: Int -> Int -> [a] -> [[a]]
    chunkInto' no ch ys
      | ch == 0 = [ys] -- length of chunks is longer than input itself
      | no == 1 = [ys] -- last sublist, put all remaining elements
      | otherwise = newChunk : chunkInto' (no - 1) ch newRemainder
      where
        newChunk = take ch ys
        newRemainder = drop ch ys

-- Problem 3

{-
  Function that maps various functions from fs over a list xs, depending on the
  index of an element in the list. The list fs of functions to be mapped is
  cycled over the list xs: the first function from fs is applied on the first
  element from xs, the second function from fs on the second element from xs,
  etc. When the list of functions fs is exhausted, mapping restarts from the
  first function from fs.
-}
cycleMap :: [a -> b] -> [a] -> [b]
cycleMap functions elements = cycleMap' 0 functions elements
  where
    fl = length functions
    cycleMap' :: Int -> [a -> b] -> [a] -> [b]
    cycleMap' _ [] _      = []
    cycleMap' _ _ []      = []
    cycleMap' n fs (x:xs) = (fs !! (n `mod` fl)) x : cycleMap' (n+1) fs xs

-- Problem 4

{-
  Explicitly recursive function that reduces a list of elements to a single
  element using a seed value and a binary reduction function.
-}
reduce :: (a -> b -> a) -> a -> [b] -> a
reduce _ seed []     = seed
reduce f seed (x:xs) = reduce f (f seed x) xs

{-
  Explicitly recursive function which behaves like reduce, but assumes the
  input list contains at least one element and so eschews taking a seed
  element.
-}
reduce1 :: (a -> a -> a) -> [a] -> a
reduce1 _ []     = error "reduce1 got an empty list"
reduce1 f (x:xs) = reduce f x xs

{-
  Explicitly recursive function that performs similarly to reduce, but returns
  a list of all the intermediate values with the result at the end instead of
  just the last result.
-}
scan :: (a -> b -> a) -> a -> [b] -> [a]
scan function seed list = seed : scan' function seed list
  where
    scan' :: (a -> b -> a) -> a -> [b] -> [a]
    scan' _ _ [] = []
    scan' f s (x:xs) = calculated : scan' f calculated xs
      where
        calculated = f s x

{-
  Variant of reduce that performs similarly, only does the operations from
  right to left, instead.
-}
rreduce :: (a -> b -> b) -> b -> [a] -> b
rreduce function seed list = reduce (flip function) seed (reverse list)

{-
  Variant of rreduce that performs similarly, but assumes the input list
  contains at least one element.
-}
rreduce1 :: (a -> a -> a) -> [a] -> a
rreduce1 _ []     = error "rreduce1 got an empty list"
rreduce1 f xs = rreduce f (last xs) (init xs)

-- Variant of the scan function that works from right to left.
rscan :: (a -> b -> b) -> b -> [a] -> [b]
rscan function seed list = reverse $ scan (flip function) seed (reverse list)

-- Problem 5

type Tolerance = Double

-- Starting approximation for square root in newton function.
startingApproximation :: Double
startingApproximation = 1

{-
  Function computes better approximation of the square root of the value with
  following arguments: value and former approximated square root.
-}
newtonsMethod :: Double -> Double -> Double
newtonsMethod approx value = (approx + (value / approx)) / 2

{-
  Function that computes an approximation of the square root of
  a number using a special case of Newton’s method.
  More can be found here: https://en.wikipedia.org/wiki/Newton%27s_method
-}
newton :: Tolerance -> Double -> Double
newton tolerance value
  | tolerance < 0 = error "Tolerance is negative"
  | value < 0 = error "Can’t get sqrt of negative number"
  | otherwise = newton' tolerance startingApproximation value
  where
    newton' :: Tolerance -> Double -> Double -> Double
    newton' tol app val
      | abs (app - computation) <= tol = computation
      | otherwise = newton' tol computation val
      where
        computation = newtonsMethod app val

-- Approximation of the derivative.
dx :: Double
dx = 0.00001

-- Function that computes the derivative of a given function.
deriv :: (Double -> Double) -> Double -> Double
deriv f x = (f (x + dx) - f x) / dx

-- Problem 6

type Operators = [(Char, Int -> Int -> Int)]

basic :: Operators
basic = [ ('+', (+)), ('-', (-)) ]

standard :: Operators
standard = [ ('+', (+)), ('-', (-)), ('*', (*)), ('/', div), ('^', (^)) ]

-- Checks whether given character is operator in specified collection of operators.
operatorExists :: Char -> Operators -> Bool
operatorExists c ops = not $ null [op | (op,_) <- ops, c == op]

-- Retrieves function linked to given operator in specified collection of operators.
operatorGet :: Char -> Operators -> (Int -> Int -> Int)
operatorGet c ops
  | operatorExists c ops = head [f | (op,f) <- ops, c == op]
  | otherwise = error ("Invalid symbol " ++ [c])

{-
  Function that takes a mathematical expression written in reverse polish
  notation and calculates its result, using the operators provided as a
  second argument to the function. The expression is limited to 1-digit
  positive integers, while the operators are always binary and of the type
  Int -> Int -> Int.
-}
rpnCalc :: String -> Operators -> Int
rpnCalc [] _ = 0
rpnCalc input operators = rpnCalc' [] input operators
  where
    -- We introduce stack-like structure which will hold our intermediate results.
    rpnCalc' :: [Int] -> String -> Operators -> Int
    -- We reached the end of our input, if stack is in invalid state - more than
    -- one number is on it, we produce an error.
    rpnCalc' stack [] _
      | length stack == 1 = head stack
      | otherwise = error "Invalid RPN expression"
    -- We consume our input
    rpnCalc' stack inp ops
      | isNumber c = rpnCalc' (digitToInt c : stack) (tail inp) ops
      | operatorExists c ops =
            if length stack < 2 then error "Invalid RPN expression"
            else
              let
              firstOp = stack !! 1
              secondOp = head stack
              operator = operatorGet c ops
              in rpnCalc' (operator firstOp secondOp : drop 2 stack) (tail inp) ops
      | otherwise = error ("Invalid symbol " ++ [c])
      where
        c = head inp

-- Problem 8

{-
  Implemented algorithm from online suggestion that permutations can be found
  by searching for the list of element's children (other elements).
  This computation is performed till we reach very bottom - element has no more
  'children'. Permutations are then formed when surfacing back from computation
  and 'scrambling' all retrieved lists of children. Scramble is performed by
  concatenating element with all possible combinations of it's children - and
  repeating this step till we reach very top.
-}

{-
  Given a list xs, it returns a list of pairs of the same length as xs, where
  each pair (y,ys) represents one selection from the list xs, i.e. an element
  from xs (y) and the rest (ys).
-}
selections :: [a] -> [(a,[a])]
selections []     = []
selections (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- selections xs]

-- Function that, given a list, returns a list of all its permutations.
permutations' :: [a] -> [[a]]
permutations' [] = [[]]
permutations' xs = [y : zs | (y,ys) <- selections xs, zs <- permutations' ys]
