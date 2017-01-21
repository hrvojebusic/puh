module Exercise3 where

-- Exercise 1.1
-- Explicitly recursive implementation.
product' :: Num a => [a] -> a
product' [] = 0
product' [x] = x
product' (x:xs) = x * product xs

-- Accumulator-style recursive implementation.
product'' :: Num a => [a] -> a
product'' = productAcc 1
  where
    productAcc :: Num a => a -> [a] -> a
    productAcc _ [] = 0
    productAcc acc [x] = acc * x
    productAcc acc (x:xs) = productAcc (acc * x) xs

-- Exercise 1.2 - Implementation with guarded recursion.
headsOf :: [[a]] -> [a]
headsOf [] = []
headsOf ([]:xss) = headsOf xss
headsOf (xs:xss) = head xs : headsOf xss

-- Exercise 2.1 - Implementation with guarded recursion.
modMult :: Integral a => a -> a -> [a] -> [a]
modMult _ 0 _ = error "Division by zero"
modMult _ _ [] = []
modMult n m (x:xs) = (v * x):modMult n m xs
  where
    v = n `mod` m

-- Exercise 2.2 - Implementation with guarded recursion.
addPredecessor :: Num a => [a] -> [a]
addPredecessor [] = []
addPredecessor (x:xs) = x:addPredecessor' x xs
  where
    addPredecessor' :: Num a => a -> [a] -> [a]
    addPredecessor' _ [] = []
    addPredecessor' v (y:ys) = (v + y):addPredecessor' y ys

-- Exercise 3.1 - Implementation with guarded recursion.
equalTriplets :: Eq a => [(a,a,a)] -> [(a,a,a)]
equalTriplets [] = []
equalTriplets (tri@(x,y,z):xs) | (x == y) && (y == z) = tri : equalTriplets xs
                               | otherwise = equalTriplets xs

-- Exercise 3.2 - Implementation with guarded recursion.
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' 1 x = [x]
replicate' n x  | n > 0 = x:replicate' (n-1) x
                | otherwise = error "Negative value given"

-- Exercise 4.1
-- Tail-recursive implementation.
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 xs = xs
drop' n (_:xs)  | n > 0 = drop' (n-1) xs
                | otherwise = error "Negative value given"

-- Explicitly recursive implementation.
drop'' :: Int -> [a] -> [a]
drop'' n xs | n < 0 = reverse $ drop' (abs n) (reverse xs)
            | otherwise = drop' n xs

-- Exercise 4.2 - Explicitly recursive implementation.
takeFromTo :: Int -> Int -> [a] -> [a]
takeFromTo _ _ [] = []
takeFromTo a b xs
  | a < 0 = error "Negative first index"
  | b < a = error "Invalid index range"
  | b >= length xs = error "Second index exceeds list length"
  | otherwise = first a (b-a+1) xs
  where
    -- Function used to ignore elements until we reach first index.
    first _ _ [] = []
    first a1 b1 l1@(_:xs1) | a1 > 0 = first (a1 - 1) b1 xs1
                           | otherwise = second b1 l1
    -- Function used to return elements until we reach second index.
    second _ [] = []
    second b2 (x2:xs2)  | b2 > 0 = x2 : second (b2 - 1) xs2
                        | otherwise = []

-- Exercise 5.1 - Implementation with guarded recursion.
eachThird :: [a] -> [a]
eachThird = eachThird' 3 3
  where
    eachThird' :: Int -> Int -> [a] -> [a]
    eachThird' _ _ [] = []
    eachThird' stp cur (x:xs) -- stp -> original step, cur -> current step value
      | cur == 1 = x : eachThird' stp stp xs
      | otherwise = eachThird' stp (cur - 1) xs

-- Exercise 5.2 - Implementation with guarded recursion.
crossZip :: [a] -> [b] -> [(a,b)]
crossZip as bs = crossZip' (zip as bs)
  where
    crossZip' :: [(a,b)] -> [(a,b)]
    crossZip' [] = []
    crossZip' [_] = []
    crossZip' ((a1,b1):((a2,b2):xs)) = (a1,b2):((a2,b1) : crossZip' xs)

-- Exercise 6.1 - Accumulator-style recursive implementation.
length' :: [a] -> Int
length' = length'' 0
  where
    length'' :: Int -> [a] -> Int
    length'' acc [] = acc
    length'' acc (_:xs) = length'' (acc + 1) xs

-- Exercise 6.2
-- Accumulator-style recursive implementation.
maxUnzip :: [(Int,Int)] -> (Int,Int)
maxUnzip [] = error "Empty list"
maxUnzip (y:ys) = maxUnzip'' y ys
  where
    maxUnzip'' :: (Int,Int) -> [(Int,Int)] -> (Int,Int)
    maxUnzip'' t [] = t
    maxUnzip'' (a1,a2) ((b1,b2):xs) = maxUnzip'' (max a1 b1, max a2 b2) xs

-- Explicitly recursive implementation.
maxUnzip' :: [(Int,Int)] -> (Int,Int)
maxUnzip' [] = error "Empty list"
maxUnzip' [x] = x
maxUnzip' ((a,b):xs) = (max a (fst recResult), max b (snd recResult))
  where
    recResult = maxUnzip' xs
