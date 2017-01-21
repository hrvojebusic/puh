import Data.List

-- Checks whether vector is null-vector.
isNullVector :: (Int, Int) -> Bool
isNullVector (x, y) = x == 0 && y == 0

-- Computes the Euclidean norm of a vector.
norm :: (Int, Int) -> Double
norm (x, y) = sqrt $ fromIntegral $ x^2 + y^2

-- Returns a normalized version of a given vector.
normalize :: (Int, Int) -> (Double, Double)
normalize (x, y)
  | isNullVector (x, y) = error "Cannot normalize null vector"
  | otherwise = (fromIntegral x / norm (x,y), fromIntegral y / norm (x,y))

-- Computes product of scalar and vector.
scalarMult :: Int -> (Int, Int) -> (Int, Int)
scalarMult a (x,y) = (a*x, a*y)

-- Computes the dot product of two vectors.
dot :: (Int, Int) -> (Int, Int) -> Int
dot (x1, y1) (x2, y2) = x1*x2 + y1*y2

-- Computes the cosine of the angle between two vectors.
cos' :: (Int, Int) -> (Int, Int) -> Double
cos' (x1, y1) (x2, y2)
  | isNullVector (x1, y1) || isNullVector (x2, y2) = error "Null vector given"
  | otherwise =
    (fromIntegral $ dot (x1, y1) (x2, y2)) /
    (norm (x1, y1) * norm (x2, y2))

-- Checks whether two given vectors are parallel.
areParallel :: (Int, Int) -> (Int, Int) -> Bool
areParallel (x1, y1) (x2, y2) =
  ((abs $ fst $ normalize (x1, y1)) == (abs $ fst $ normalize (x2, y2))) &&
  ((abs $ snd $ normalize (x1, y1)) == (abs $ snd $ normalize (x2, y2)))

{-
Splits the list at index n and returns a tuple with the first element
containing the first part, and the second element containing the second
part of the list.
-}
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs
  | null xs = error "List given is empty"
  | n < 1 || n >= length xs = error "N is out of range"
  | otherwise = (take n xs, drop n xs)

-- Decomposes number to it's digits in reverse order.
numToDigitsRev :: Int -> [Int]
numToDigitsRev n
  | abs n < 10 = [n]
  | otherwise = ((abs n) `mod` 10):(numToDigitsRev $ (abs n) `div` 10)

-- Decomposes number to it's digits.
numToDigits :: Int -> [Int]
numToDigits n = reverse $ numToDigitsRev n

-- Decomposes number given as String to it's digits.
stringToDigits :: [Char] -> [Int]
stringToDigits xs = numToDigits (read xs :: Int)

-- Implementation of Luhn algorithm: https://en.wikipedia.org/wiki/Luhn_algorithm
luhn :: [Char] -> Bool
luhn xs
  | null xs = error "Empty string given"
  | (read xs :: Int) < 0 = error "Negative number was passed"
  | otherwise = sum [sum $ numToDigits x | x <-
    [if even $ fst x then snd x * 2 else snd x | x <-
      zip [1..] (reverse $ stringToDigits xs)]] `mod` 10 == 0

-- Decomposes number and returns it's prime factors.
factorize :: Int -> [Int]
factorize x = factorizeHelp 2 (abs x)

{-
  Helper function performs actual decomposition of number on it's prime factors.
  First argument is candidate for prime factor, second argument is number to be
  decomposed on it's prime factors.
-}
factorizeHelp :: Int -> Int -> [Int]
factorizeHelp n x
  | x == 1 = []
  | x `mod` n == 0 = n : factorizeHelp n (x `div` n)
  | otherwise = factorizeHelp (n+1) x

-- Produces first N prime numbers, N is passed as function argument.
primes :: Int -> [Int]
primes n
  | n < 1 = error "Invalid number of primes requested"
  | otherwise = primesHelp n 2

{-
  Helper function produces N prime numbers. Number of primes to be produced
  is defined by first argument and prime candidate (number from which primes
  will be considered) is passed as second argument.
-}
primesHelp :: Int -> Int -> [Int]
primesHelp n x
  | n == 0 = []
  | (x == 2 || x `mod` 2 /= 0) && length (factorize x) == 1 =
    x : primesHelp (n-1) (x+1)
  | otherwise = primesHelp n (x+1)
