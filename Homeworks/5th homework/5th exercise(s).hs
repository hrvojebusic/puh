module Exercise5 where

import           Data.Char
import           Data.List
import           Data.Ord   (comparing)
import           Data.Tuple

---------------
--7th lecture--
---------------

-- Exercise 1.1.1
takeThree :: [a] -> [a]
takeThree = take 3

-- Exercise 1.1.2
dropThree :: [a] -> [a]
dropThree = drop 3

-- Exercise 1.1.3
hundredTimes :: a -> [a]
hundredTimes = replicate 100

-- Exercise 1.2.1
index :: [a] -> [(Int, a)]
index = zip [0..]

-- Exercise 1.2.2
index' :: [a] -> [(a,Int)]
index' = flip zip [0..]

-- Exercise 1.3
divider :: Int -> String
divider = flip replicate '='

-- Exercise 2.1.1
applyOnLast :: (a -> b -> c) -> [a] -> [b] -> c
applyOnLast _ [] _  = error "Empty first list"
applyOnLast _ _ []  = error "Empty second list"
applyOnLast f xs ys = f (last xs) (last ys)

-- Exercise 2.1.2
addThree :: Num a => a -> a -> a -> a
addThree x y z = x + y + z

-- Exercise 2.1.3
lastTwoPlus100 :: [Integer] -> [Integer] -> Integer
lastTwoPlus100 = applyOnLast (addThree 100)

-- Exercise 2.2.1
applyManyTimes :: Int -> (Int -> Int) -> Int -> Int
applyManyTimes n f x
  | n <= 0 = x
  | n == 1 = f x
  | otherwise = f $ applyManyTimes (n - 1) f x

-- Exercise 2.2.2
applyTwice :: (Int -> Int) -> Int -> Int
applyTwice = applyManyTimes 2

-- Exercise 3.1
listifyList :: [a] -> [[a]]
listifyList = map (:[])

-- Exercise 3.2
cutoff :: Int -> [Int] -> [Int]
cutoff n = map (\x -> if x < n then x else n)

-- Exercise 4.1
sumEvenSquares :: [Integer] -> Integer
sumEvenSquares xs = foldl (\ a x -> a + x * x) 0 $ filter even xs

-- Exercise 4.2
freq :: Eq a => a -> [a] -> Int
freq x xs = length $ filter (==x) xs

-- Exercise 4.3
freqFilter :: Eq a => Int -> [a] -> [a]
freqFilter n xs = filter (\ x -> freq x xs >= n) xs

-- Exercise 5.1
withinInterval :: (Ord a) => a -> a -> [a] -> [a]
withinInterval _ _ [] = []
withinInterval n m xs
  | n > m = error "Invalid interval"
  | otherwise = filter (\x -> x >= n && x <= m) xs

-- Exercise 5.2
-- Checks whether the matrix has all rows of equal length.
isWellFormed :: [[a]] -> Bool
isWellFormed []     = False
isWellFormed ([]:_) = False
isWellFormed [_]    = True
isWellFormed xss    = length (nub [length xs | xs <- xss]) == 1

sndColumn :: [[a]] -> [a]
sndColumn xss
  | isWellFormed xss && length (head xss) >= 2 = map (\(_:x:_) -> x) xss
  | otherwise = error "Matrix is not valid"

-- Exercise 5.3
canonicalizePairs :: (Ord a) => [(a,a)] -> [(a,a)]
canonicalizePairs [] = []
canonicalizePairs xs =
  map (\(a,b) -> (min a b, max a b)) (filter (uncurry (/=)) xs)

---------------
--8th lecture--
---------------

-- Exercise 1.1.
sumEven :: [Integer] -> Integer
sumEven = sum . map snd . filter (even . fst) . zip [0..]

-- Exercise 1.2.
filterWords :: [String] -> String -> String
filterWords ws = unwords . filter (`notElem` ws) . words

-- Exercise 1.3.1.
initials3 :: String -> (String -> Bool) -> String -> String
initials3 d p = foldl (\acc e -> acc ++ [e] ++ d) [] . map (toUpper . head) . filter p . words

-- Exercise 1.3.2.
initials :: String -> String
initials = initials3 "." (const True)

-- Function returns list of deltas between each two elements of the list.
diff :: [Integer] -> [Integer]
diff []       = error "Empty list"
diff l@(_:xs) = map (abs . uncurry (-)) $ zip l xs

-- Exercise 2.1.1
maxDiff :: [Integer] -> Integer
maxDiff = maximum . diff

-- Exercise 2.1.2
maxMinDiff :: [Integer] -> (Integer, Integer)
maxMinDiff xs = (maximum deltas, minimum deltas)
  where
    deltas = diff xs

-- Student's name and surname.
type NameSurname = String
-- Student's score.
type Score = Double

-- Exercise 2.2
studentsPassed :: [(NameSurname, Score)] -> [(NameSurname, Score)]
studentsPassed xs = filter (\(_,s) -> s >= 0.5 * best) xs
  where
    best :: Score
    best = fst . maximum $ map swap xs

-- Exercise 3.1.
isTitleCased :: String -> Bool
isTitleCased = not . any (isLower . head) . words

-- Exercise 3.2.
sortPairs :: Ord a => [(a,a)] -> [(a,a)]
sortPairs = map swap . sort . map swap

-- Exercise 3.3.
filename :: String -> String
filename [] = error "Empty String"
filename xs
  | '/' `elem` xs = drop (lastIndexOf + 1) xs
  | otherwise = xs
  where
    lastIndexOf = last $ elemIndices '/' xs

-- Exercise 3.4.
maxElemIndices :: Ord a => [a] -> [Int]
maxElemIndices [] = error "Empty list"
maxElemIndices xs = elemIndices maxElem xs
  where
    maxElem = maximum xs

-- Exercise 4.1.
elem' :: Eq a => a -> [a] -> Bool
elem' x = foldr (\a ac -> a == x || ac) False

-- Exercise 4.2.
reverse' :: [a] -> [a]
reverse' = foldr (\el acc -> acc ++ [el]) []

-- Exercise 4.3.
nubRuns :: Eq a => [a] -> [a]
nubRuns [] = []
nubRuns xs =
  foldr (\el acc -> if el == head acc then acc else el:acc) [last xs] (init xs)

-- Exercise 5.1.
reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

-- Exercise 5.2.
sumEven' :: [Integer] -> Integer
sumEven' =
  foldl (\acc el -> if even (fst el) then acc + snd el else acc) 0 . zip [0..]

-- Exercise 5.3.
maxUnzip :: [(Int,Int)] -> (Int,Int)
maxUnzip [] = error "Empty list"
maxUnzip zs = foldr (\(x1,y1) (x2,y2) -> (max x1 x2, max y1 y2)) (last zs) zs

---------------
--9th lecture--
---------------

-- Exercise 1.1.
data Date = Date
  { day   :: Int
  , month :: Int
  , year  :: Int }

showDate :: Date -> String
showDate (Date da mo yr) = show da ++ "." ++ show mo ++ "." ++ show yr ++ "."

-- Exercise 1.2.
data Point = Point
  { x :: Double
  , y :: Double } deriving Show

-- Circle is defined by it's center point and radius.
-- Rectangle is defined by it's bottom left an top right corner (points).
data Shape2 = Circle2 Point Double | Rectangle2 Point Point deriving Show

translate :: Point -> Shape2 -> Shape2
translate (Point x' y') (Circle2 p r) =  Circle2 (Point (x p + x') (y p + y')) r
translate (Point x' y') (Rectangle2 p1 p2) =
  Rectangle2 (Point (x p1 + x') (y p1 + y')) (Point (x p2 + x') (y p2 + y'))

-- Exercise 1.3.
-- Calculates distance between two points.
distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) =
  sqrt ((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2))

toAbs :: Point -> Point
toAbs p = Point (abs $ x p) (abs $ y p)

inShape :: Shape2 -> Point -> Bool
inShape (Circle2 p1 r) p2 = distance p1 p2 <= r
inShape (Rectangle2 p1 p2) p3 =
  x p1 <= x p3 && x p3 <= x p2 && y p1 <= y p3 && y p3 <= y p2

-- Exercise 1.4.
data Vehicle =
  Car String Double
  | Truck String Double
  | Motorcycle String Double
  | Bicycle

-- Bicycle horsepower.
bh :: Double
bh = 0.2

totalHorsepower :: [Vehicle] -> Double
totalHorsepower []                  = 0
totalHorsepower (Bicycle:xs)        = bh + totalHorsepower xs
totalHorsepower (Car _ h:xs)        = h + totalHorsepower xs
totalHorsepower (Truck _ h:xs)      = h + totalHorsepower xs
totalHorsepower (Motorcycle _ h:xs) = h + totalHorsepower xs

-- Exercise 2.1.1.
data Level    = Bachelor | Master | PhD deriving (Show,Eq)

data Student = Student
  { firstName :: String
  , lastName  :: String
  , studentId :: String
  , level     :: Level
  , avgGrade  :: Double } deriving Show

improveStudent :: Student -> Student
improveStudent s
  | tempGrade <= 5.0 = s { avgGrade = tempGrade }
  | otherwise = s
  where
    tempGrade = avgGrade s + 1.0

-- Exercise 2.1.2.
improveStudent' :: Student -> Student
improveStudent' s@Student {avgGrade = g} = s {avgGrade = min 5 (g + 1)}

-- Exercise 2.2.1.
average :: (Real a, Fractional b) => [a] -> b
average [] = 0
average xs = realToFrac (sum xs) / genericLength xs

-- Exercise 2.2.2.
avgGradeForCondition :: (Student -> Bool) -> [Student] -> Double
avgGradeForCondition f = average . map avgGrade . filter f

-- Exercise 2.2.3.
avgGradePerLevels :: [Student] -> (Double,Double,Double)
avgGradePerLevels xs = (b,m,p)
  where
    b = avgGradeForCondition (\s -> level s == Bachelor) xs
    m = avgGradeForCondition (\s -> level s == Master) xs
    p = avgGradeForCondition (\s -> level s == PhD) xs

-- Exercise 2.3.1.
sortByAvgGrade :: [Student] -> [Student]
sortByAvgGrade =  sortBy (flip (comparing avgGrade))

-- Exercise 2.3.2.
rankedStudents :: Level -> [Student] -> [String]
rankedStudents l = map studentId . sortByAvgGrade . filter (\s -> level s == l)

-- Exercise 2.4.1.
studentPresent :: String -> [Student] -> Bool
studentPresent sid = any (\s -> studentId s == sid)

-- Exercise 2.4.2.
addStudent :: Student -> [Student] -> [Student]
addStudent s xs
  | studentPresent (studentId s) xs = error "Student is already present"
  | otherwise = s:xs

-- Exercise 3.1.
data MyTriplet a b c = MyTriplet
  { first  :: a
  , second :: b
  , third  :: c }

toTriplet :: MyTriplet a b c -> (a,b,c)
toTriplet (MyTriplet f s t) = (f,s,t)

-- Exercise 3.2.
data Employee = Employee
   { name   :: String
   , salary :: Maybe Double } deriving Show

totalSalaries :: [Employee] -> Double
totalSalaries = totalSalaries' 0

totalSalaries' :: Double -> [Employee] -> Double
totalSalaries' acc [] = acc
totalSalaries' acc (e:xs) =
  case salary e of
    Just n  -> totalSalaries' (acc + n) xs
    Nothing -> totalSalaries' acc xs

-- Exercise 3.3.1.
addStudent2 :: Student -> [Student] -> Maybe [Student]
addStudent2 s xs
  | studentPresent (studentId s) xs = Nothing
  | otherwise = Just (s:xs)

-- Exercise 3.3.2.
addStudent3 :: Student -> [Student] -> Either String [Student]
addStudent3 s xs
  | studentPresent (studentId s) xs = Left "Student is already present"
  | otherwise = Right (s:xs)
