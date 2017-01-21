import Data.Char
import Data.List

-- LECTURE 1
-- Exercise 1.1
concat3 :: String -> String -> String -> String
concat3 s1 s2 s3 = if length s2 < 2 then s1 ++ s3 else s1 ++ s2 ++ s3

-- Exercise 1.2
-- Seemed more appropriate to show error where salary or bonus is negative.
showSalary :: Int -> Int -> String
showSalary amount bonus
   | amount < 0 = error "Salary is negative"
   | bonus < 0 = error "Bonus is negative"
   | otherwise = "Salary is " ++ show amount ++
                  if bonus /= 0 then ", and a bonus " ++ show bonus else ""

-- LECTURE 2
-- Exercise 1.1
-- Improvment upon requested function.
rmvNFstLst :: Int -> [a] -> [a]
rmvNFstLst n xs = drop n $ reverse $ drop n $ reverse xs

rmvFstLst :: [a] -> [a]
rmvFstLst xs = rmvNFstLst 3 xs

-- Exercise 1.2
initials :: String -> String -> String
initials s1 s2
  | null s1 || null s2 = error "Cannot construct initials"
  | otherwise = [toUpper $ s1 !! 0] ++ ". " ++ [toUpper $ s2 !! 0] ++ "."

-- Exercise 1.3
concat' :: String -> String -> String
concat' s1 s2 = if length s1 >= length s2 then s1 ++ s2 else s2 ++ s1

-- Exercise 1.4
safeHead :: [a] -> [a]
safeHead xs = if null xs then xs else [xs !! 0]

-- Exercise 1.5
hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates xs = length xs /= (length $ nub xs)

-- Exercise 2.1
doublesFromTo :: Int -> Int -> [Int] -> [Int]
doublesFromTo a b xs
  | a > b = doublesFromTo b a xs
  | otherwise = [2 * snd x | x <- zip [1..] xs, fst x >= a && fst x <= b]

-- Exercise 2.2
caesarCode :: Int -> String -> String
caesarCode n xs = [
    if isLetter b then chr ((ord b - 97 + n) `mod` 26 + 97)
    else b
    | b <- [toLower a | a <- concat $ words xs]]

-- Exercise 3.1
letterCount :: String -> Int
letterCount xs = length [y | y <-
  concat [x | x <- words xs, length x > 2], isLetter y]

-- Exercise 3.2
flattenString :: String -> String
flattenString xs = [toLower x | x <- xs, x /= ' ']

isPalindrome :: String -> Bool
isPalindrome xs = flattenString xs == (reverse $ flattenString xs)

-- Exercise 3.3
flipp :: [String] -> String
flipp xss = concat $ reverse [reverse x | x <- xss]

-- Exercise 4.1
{-
  Coordinates marked with number 1 represent circle center with radius r.
  Coordinates marked with number 2 represent point which is tested
  to be inside the circle.
-}
isInCircle :: Double -> Int -> Int -> Int -> Int -> Bool
isInCircle r x1 y1 x2 y2 = sqrt(fromIntegral $ (x1-x2)^2 + (y1-y2)^2) <= r

inCircle :: Double -> Int -> Int -> [(Int, Int)]
inCircle r x1 y1 = [(x, y)
  | x <- [-10..10], y <- [-10..10], isInCircle r x1 y1 x y]

inCircle' :: Double -> Int -> Int -> [Int] -> [Int] -> [(Int, Int)]
inCircle' r x1 y1 xAxis yAxis =
 [(x, y) | x <- xAxis, y <- yAxis, isInCircle r x1 y1 x y]

-- Exercise 4.2
steps :: [a] -> [(a,a)]
steps [] = []
steps [x] = []
steps xs = zip xs (tail xs)

-- Exercise 5.1
indices :: Char -> String -> [Int]
indices x xs = [ fst ix | ix <- zip [0..] xs, snd ix == x]

-- Exercise 5.2
showLineNumbers :: String -> String
showLineNumbers s = concat [(show $ fst l) ++ snd l ++ "\n" | l <- zip [1..] (lines s)]

-- Exercise 5.3
common :: String -> String -> String
common xs ys = [fst x | x <- zip xs ys, fst x == snd x]

haveAlignment :: String -> String -> Bool
haveAlignment xs ys = not $ null $ common xs ys
