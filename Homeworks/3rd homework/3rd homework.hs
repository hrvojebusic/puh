module Homework3 where

import Text.Regex.Posix

-- Problem 1
type RomanNumeral = String

{-
  Checks whether the given numeral consists only of valid literals (only letters
  ‘I’, ‘V’, ‘X’, ‘L’, ‘C’, ‘D’, ‘M’) and whether it follows the rules of writing.
-}
isValidRoman :: RomanNumeral -> Bool
isValidRoman [] = False
isValidRoman xs = xs =~ "^M{0,3}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})$" :: Bool

-- Dictionary used for composition of Roman numeral from passed integer.
toRomanDictionary :: [(Int,RomanNumeral)]
toRomanDictionary = [(1000,"M"),(900,"CM"),(500,"D"),(400,"CD"),(100,"C"),(90,"XC"),
                    (50,"L"),(40,"XL"),(10,"X"),(9,"IX"),(5,"V"),(4,"IV"),(1,"I")]

-- Converts the given number to Roman notation. Numbers supported are [1,3999].
toRoman :: Int -> RomanNumeral
toRoman x
  | x <= 0 || x >= 4000 = error "Number can not be represented"
  | otherwise = toRoman' toRomanDictionary "" x

-- Accumulator-style recursive implementation of toRoman function.
toRoman' :: [(Int,RomanNumeral)] -> RomanNumeral -> Int -> RomanNumeral
toRoman' [] r _ = r -- we reached the end of our dictionary, decomposition is finished
-- r -> resulting string, n -> number we are decomposing
toRoman' ((a,b):xs) r n = toRoman' xs newR newN
  where
    count = n `div` a
    newR = r ++ concat (replicate count b)
    newN = n - (count * a)

-- Dictionary used for decomposing passed Roman numeral and forming of Integer.
fromRomanDictionary :: [(Char,Int)]
fromRomanDictionary = [('M',1000),('D',500),('C',100),
                      ('L',50),('X',10),('V',5),('I',1)]

-- Returns the value associated with the given key in fromRomanDictionary.
get :: [(Char,Int)] -> Char -> Int
get dic k
  | not $ null search = head search
  | otherwise = error "Not supported roman letter"
  where
    search = [value | (key,value) <- dic, key == k]

-- Converts the Roman numeral to a decimal representation.
fromRoman :: RomanNumeral -> Int
fromRoman xs
  | isValidRoman xs = fromRoman' 0 xs
  | otherwise = error "Not a valid Roman numeral"

-- Accumulator-style recursive implementation of fromRoman function.
fromRoman' :: Int -> RomanNumeral -> Int
fromRoman' acc [] = acc
fromRoman' acc [a] = acc + value
  where
    value = get fromRomanDictionary a
fromRoman' acc l@(a:b:_) = fromRoman' newAcc (tail l)
  where
    value = get fromRomanDictionary a
    nextValue = get fromRomanDictionary b
    newAcc = if value >= nextValue then acc + value else acc - value

-- Problem 2
{-
  Since problem description does not specify which algorithm should be
  utilized while looking for shortest path, this solution relies on 'greedy
  algorithm' for best -average- performance.
-}

-- Takes tuple closest to specified tuple from given list.
takeShortest :: (Int, Int) -> [(Int,Int)] -> (Int, Int)
takeShortest _ [] = error "Empty list"
takeShortest (a,b) l@(x:xs)
  | length l == 1 = x
  | otherwise = closer x (takeShortest (a,b) xs)
  where
    closer (c,d) (e,f)
      | fstDist <= sndDist = (c,d)
      | otherwise = (e,f)
      where
        fstDist = abs (c-a) + abs (d-b)
        sndDist = abs (e-a) + abs (f-b)

-- Removes specified tuple from list, if such exists in the list.
removeTaken :: (Int, Int) -> [(Int,Int)] -> [(Int,Int)]
removeTaken _ [] = []
removeTaken tuple (x:xs)
  | tuple == x = xs
  | otherwise = x:removeTaken tuple xs

-- Calculates distance between two coordinates.
getDistance :: (Int,Int) -> (Int,Int) -> Int
getDistance (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

{-
  Calculates shortest distance traveled when needed to visit all the nodes
  in the graph, with provided starting position.
-}
shortestDistance :: (Int, Int) -> [(Int, Int)] -> Int
shortestDistance _ [] = 0
shortestDistance root xs =
  shortestDistance' startDistance root closestNode newList
  where
    closestNode = takeShortest root xs
    startDistance = getDistance root closestNode
    newList = removeTaken closestNode xs

shortestDistance' :: Int -> (Int,Int) -> (Int,Int)-> [(Int, Int)] -> Int
-- Case when we reached last node in the graph
shortestDistance' tr root close [] = tr + getDistance root close
shortestDistance' tr root close xs =
  shortestDistance' (tr + newDistance) root closestNode newList
  where
    closestNode = takeShortest close xs
    newDistance = getDistance close closestNode
    newList = removeTaken closestNode xs

-- Problem 3
type Probability = Double
type DiscreteRandVar = [(Int, Probability)]

xVector :: DiscreteRandVar
xVector = [(1, 0.2), (2, 0.4), (3, 0.1), (4, 0.2), (5, 0.05), (6, 0.05)]

-- Computes mean for single value inside discrete random variable vector.
meanCompute :: (Int, Probability) -> Double
meanCompute (v,prob) = fromIntegral v * prob

-- Mean function calculates the mean (expected value) of a discrete random variable.

-- Explicitly recursive implementation.
mean :: DiscreteRandVar -> Double
mean []     = error "Empty vector"
mean [x]    = meanCompute x
mean (x:xs) = meanCompute x + mean xs

-- Accumulator-style recursive implementation.
mean' :: DiscreteRandVar -> Double
mean' = meanAccumulator 0
  where
    meanAccumulator :: Double -> DiscreteRandVar -> Double
    meanAccumulator _ []     = error "Empty vector"
    meanAccumulator m [x]    = m + meanCompute x
    meanAccumulator m (x:xs) = meanAccumulator (m + meanCompute x) xs

-- Computes variance for single value inside discrete random variable vector.
varianceCompute :: Double -> (Int, Probability) -> Double
varianceCompute m (v,prob) = (fromIntegral v - m)^2 * prob

-- Variance function calculates the variance of a discrete random variable.

-- Explicitly recursive implementation.
variance :: DiscreteRandVar -> Double
variance y = varianceNonAccumulator (mean y) y
  where
    varianceNonAccumulator :: Double -> DiscreteRandVar -> Double
    varianceNonAccumulator _ [] = error "Empty vector"
    varianceNonAccumulator m [x] = varianceCompute m x
    varianceNonAccumulator m (x:xs) =
      varianceCompute m x + varianceNonAccumulator m xs

-- Accumulator-style recursive implementation.
variance' :: DiscreteRandVar -> Double
variance' y = varianceAccumulator 0 (mean' y) y
  where
    varianceAccumulator :: Double -> Double -> DiscreteRandVar -> Double
    varianceAccumulator _ _ [] = error "Empty vector"
    varianceAccumulator v m [x] = v + varianceCompute m x
    varianceAccumulator v m (x:xs) =
      varianceAccumulator (v + varianceCompute m x) m xs

{-
  ProbabilityFilter function takes a probability and a random variable, and
  returns a list of values that have at least the given probability of appearing.
-}

-- Explicitly recursive implementation - uses guarded recursion.
probabilityFilter :: Probability -> DiscreteRandVar -> [Int]
probabilityFilter _ [] = []
probabilityFilter v (x:xs)
  | v < 0 = error "Probability filter is negative"
  | snd x >= v = fst x : t
  | otherwise = t
  where
    t = probabilityFilter v xs

{-
  Accumulator-style recursive implementation - worse solution in this case
  beacuse it constructs resulting list in reverse order, so one more traversal
  of resulting list is needed.
-}
probabilityFilter' :: Probability -> DiscreteRandVar -> [Int]
probabilityFilter' _ [] = []
probabilityFilter' bound list
  | bound < 0 = error "Probability filter is negative"
  | otherwise = reverse $ pf bound list []
  where
    pf _ [] r = r
    pf v (x:xs) r
      | snd x >= v = pf v xs (fst x : r)
      | otherwise = pf v xs r
