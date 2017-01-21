module Exercise6 where

import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO
import           System.Random

----------------
--10th lecture--
----------------

-- Exercise 1.
data Sex = Male | Female deriving (Show,Read,Ord,Eq)

data Person2 = Person2 {
   personId2 :: String,
   forename2 :: String,
   surname2  :: String,
   sex2      :: Sex,
   mother2   :: Maybe Person2,
   father2   :: Maybe Person2,
   partner2  :: Maybe Person2,
   children2 :: [Person2] } deriving (Read,Eq,Ord)

instance Show Person2 where
  show p = forename2 p ++ " " ++ surname2 p

-- Exercise 1.2.1.
amIChild :: Person2 -> Maybe Person2 -> Bool
amIChild child parent = case parent of
                Just p  -> child `elem` children2 p
                Nothing -> False

-- Exercise 1.2.2.
parentCheck :: Person2 -> Bool
parentCheck p = amIChild p (mother2 p) || amIChild p (father2 p)

-- Exercise 1.3.1.
getChildren :: Maybe Person2 -> [Person2]
getChildren parent = case parent of
                      Just p  -> children2 p
                      Nothing -> []

-- Exercise 1.3.2.
sister :: Person2 -> Maybe Person2
sister p = if null sisters then Nothing else Just (head sisters)
  where
    sisters = filter  (\c -> sex2 c == Female)
                      (getChildren (mother2 p) ++ getChildren (father2 p))

-- Exercise 1.4.
descendant :: Person2 -> [Person2]
descendant p
  | null c = []
  | otherwise = c ++ concatMap descendant c
  where
    c = children2 p

-- Persons used for testing.
jack :: Person2
jack = Person2 "123" "Jack" "Doe" Male Nothing Nothing (Just jane) [alice, jennifer]
jane :: Person2
jane = Person2 "623" "Jane" "Fox" Female (Just ann) Nothing (Just jack) [alice, jennifer]
ann :: Person2
ann  = Person2 "343" "Ann"  "Doe" Female Nothing Nothing Nothing [jane]
alice :: Person2
alice  = Person2 "443" "Alice"  "Doe" Female (Just jane) (Just jack) Nothing []
jennifer :: Person2
jennifer  = Person2 "343" "Jennifer"  "Doe" Female (Just jane) (Just jack) Nothing []

-- Exercise 2.
data MyList a = Empty | Cons a (MyList a) deriving (Show, Read, Ord)

infixr 5 -+-
(-+-) :: a -> MyList a -> MyList a
(-+-) = Cons

-- Exercise 2.1.
listHead :: MyList a -> Maybe a
listHead Empty        = Nothing
listHead (a `Cons` _) = Just a

-- Exercise 2.2.
listMap :: (a -> b) -> MyList a -> MyList b
listMap _ Empty        = Empty
listMap f (a `Cons` l) = f a `Cons` listMap f l

-- Exercise 3.
data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show,Read,Ord)

-- Exercise 3.1.
treeMax :: Ord a => Tree a -> a
treeMax t = fromMaybe (error "Tree is empty") (treeMax' t)

treeMax' :: Ord a => Tree a -> Maybe a
treeMax' Null = Nothing
treeMax' (Node x left right) = maxM (Just x) $ maxM (treeMax' left) (treeMax' right)

-- Implementation of max function which works with Maybe.
maxM :: (Ord a, Eq a) => Maybe a -> Maybe a -> Maybe a
maxM (Just x) (Just y) = Just (max x y)
maxM e@(Just _) _      = e
maxM _ e@(Just _)      = e
maxM _ _               = Nothing

-- Exercise 3.2.
treeToList :: Ord a => Tree a -> [a]
treeToList = treeToList' []

treeToList' :: Ord a => [a] -> Tree a -> [a]
treeToList' l Null = l
treeToList' l (Node x left right) = treeToList' l left ++ [x] ++ treeToList' l right

-- Exercise 3.3.
levelCut :: Int -> Tree a -> Tree a
levelCut _ Null = Null
levelCut n (Node x left right)
  | n < 0 = error "Negative level"
  | n == 0 = Null
  | otherwise = Node x (levelCut (n-1) left) (levelCut (n-1) right)

-- Exercise 4.1.1.
treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x Null = Node x Null Null
treeInsert x t@(Node y l r)
  | x < y     = Node y (treeInsert x l) r
  | x > y     = Node y l (treeInsert x r)
  | otherwise = t

-- Exercise 4.1.2.
listToTree :: Ord a => [a] -> Tree a
listToTree = foldr treeInsert Null

-- Exercise 4.2.
sortAndNub :: Ord a => [a] -> [a]
sortAndNub = treeToList . listToTree

-- Exercise 5.1.
data Weekday =
   Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
   deriving (Show,Enum)

instance Eq Weekday where
  Monday    == Monday    = True
  Tuesday   == Tuesday   = True
  Wednesday == Wednesday = True
  Thursday  == Thursday  = True
  Saturday  == Saturday  = True
  Sunday    == Sunday    = True
  _         == _         = False

-- Exercise 5.2.
data Person' = Person' {
   idNumber' :: String,
   forename' :: String,
   surname'  :: String,
   sex'      :: Sex,
   age'      :: Int,
   partner'  :: Maybe Person',
   children' :: [Person'] } deriving (Read,Ord,Eq)

instance Show Person' where
  show = forename'

-- We have now solved problem of printing infinite structures by
-- defining manually 'Person'' as instance of 'Show

-- Persons used for testing.
pero :: Person'
pero  = Person' "2323" "Pero"  "Perić" Male   45 (Just ana)   [marko]
ana :: Person'
ana   = Person' "3244" "Ana"   "Anić"  Female 43 (Just pero)  [marko,iva]
marko :: Person'
marko = Person' "4341" "Marko" "Perić" Male   22 (Just maja)  []
maja :: Person'
maja  = Person' "7420" "Maja"  "Majić" Female 20 (Just marko) []
iva :: Person'
iva   = Person' "4642" "Iva"   "Ivić"  Female 16 Nothing      []

-- Executing printPersons now will not cause infinite loop.
printPersons :: String
printPersons = show pero ++ ", " ++ show ana

-- Exercise 6.1.
instance (Eq a) => Eq (MyList a) where
  Cons a _ == Cons b _ = a == b
  _ == _ = False

-- Exercise 6.2.
instance (Eq a, Ord a) => Eq (Tree a) where
  t1 == t2 = sortAndNub (treeToList t1) == sortAndNub (treeToList t2)

----------------
--11th lecture--
----------------

-- Exercise 1.1.
twoStrings :: IO ()
twoStrings = do
  strings <- replicateM 2 getLine
  putStrLn "Two strings concatenated and reversed: "
  putStrLn $ reverse $ concat strings

-- Exercise 1.2.
threeNumbers :: IO ()
threeNumbers = do
  numbers <- replicateM 3 getLine
  putStrLn "Sum of three numbers: "
  print $ sum $ map (\n -> read n :: Int) numbers

-- Exercise 2.1.
threeStrings :: IO Int
threeStrings = do
  strings <- replicateM 3 getLine
  putStrLn ("Input : " ++ unwords strings)
  return $ length $ concat strings

-- Exercise 2.2.
askNumber9 :: IO Int
askNumber9 = do
  line <- getLine
  let
    number = read line :: Int
  if number == 9 then return 9 else askNumber9

-- Exercise 2.3.1.
repeatInput :: (String -> Bool) -> IO String
repeatInput p = do
  l <- getLine
  if p l
    then do
      putStr $ "Input that satisfies: " ++ l
      return l
    else repeatInput p

-- Exercise 2.3.2.
askUser :: String -> (String -> Bool) -> IO String
askUser m p = do
  putStrLn m
  repeatInput p

-- Exercise 2.3.3
repeatInput' :: Read a => (String -> Bool) -> IO a
repeatInput' p = do
  l <- getLine
  if p l
    then do
      putStr $ "Input that satisfies: " ++ l
      return $ read l
    else repeatInput' p

-- Exercise 2.3.4.
askUser' :: Read a => String -> (String -> Bool) -> IO a
askUser' m p = do
  putStrLn m
  repeatInput' p

-- Exercise 2.4.
inputStrings :: IO [String]
inputStrings = do
  line <- getLine
  if null line
    then return []
    else do
      r <- inputStrings
      return (line : r)

-- Exercise 3.1.
stringReversal :: IO ()
stringReversal = do
  putStrLn "Number of strings:"
  l <- getLine
  let
    n = read l :: Int
  ys <- forM [1..n] $ \x -> do
    putStrLn $ "Input " ++ show x ++ "th string"
    getLine
  putStrLn $ "The result is: " ++ unwords (reverse ys)

-- Exercise 3.2.1.
sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (m:ms) = k m (sequence' ms)
   where
     k mo mo' = do { x <- mo; xs <- mo'; return (x:xs) }

-- Exercise 3.2.2.
sequence_' :: Monad m => [m a] -> m ()
sequence_' [] = return ()
sequence_' (m:ms) = k m (sequence_' ms)
  where
    k mo mo' = do { _ <- mo; _ <- mo'; return () }

-- Exercise 3.3.1.
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f as = sequence' (map f as)

-- Exercise 3.3.2.
mapM_' :: Monad m => (a -> m b) -> [a] -> m ()
mapM_' f as = sequence_' (map f as)

-- Exercise 3.4.
pythagoreanTriplets :: IO ()
pythagoreanTriplets =
  forM_ [(a,b,c) | a <- [1..100], b <- [1..100], c <- [1..100]] $ \(a,b,c) ->
    when (a * a + b * b == c * c) $
      putStrLn $ "Triplet: " ++ show (a,b,c)

-- Exercise 4.1.
filterOdd :: IO ()
filterOdd =
  interact (unlines . map snd . filter (even . fst) . zip [0..] . lines)

-- Exercise 4.2.
numberLines :: IO ()
numberLines =
  interact (unlines . map (\(n,c) -> show n ++ ". " ++ c) . zip [1..] . lines)

-- Exercise 4.3.
filterWords :: [String] -> IO ()
filterWords set =
  interact (unlines . map (unwords . filter (`notElem` set) . words) . lines)

-- Exercise 5.1.
wc :: FilePath -> IO (Int, Int, Int)
wc f = do
  h <- openFile f ReadMode
  s <- hGetContents h
  let
    nCs = length s
    nWs = length $ words s
    nLs = length $ lines s
  hClose h
  return (nCs, nWs, nLs)

-- Exercise 5.2.
copyLines :: [Int] -> FilePath -> FilePath -> IO ()
copyLines xs f1 f2 = do
  h1 <- openFile f1 ReadMode
  h2 <- openFile f2 WriteMode
  s <- hGetContents h1
  hPutStr h2
    $ unlines . map snd . filter (\t -> fst t `elem` xs) $ zip [1..] $ lines s
  hClose h1
  hClose h2

-- Exercise 6.1.
wordTypes :: FilePath -> IO Int
wordTypes f = do
  c <- readFile f
  return (length . nub $ words c)

-- Exercise 6.2.1.
printDiff :: [String] -> [String] -> IO()
printDiff [] [] = return ()
printDiff (xs:xss) [] = do
  putStrLn $ "< " ++ xs
  printDiff xss []
printDiff [] (xs:xss) = do
  putStrLn $ "> " ++ xs
  printDiff [] xss
printDiff (xs:xss) (ys:yss) =
  if xs /= ys
    then do
      putStrLn $ "< " ++ xs
      putStrLn $ "> " ++ ys
      printDiff xss yss
    else
      printDiff xss yss

-- Exercise 6.2.2.
diff :: FilePath -> FilePath -> IO ()
diff f1 f2 = do
  c1 <- readFile f1
  c2 <- readFile f2
  printDiff (lines c1) (lines c2)

-- Exercise 6.3.1.
removeTrailing :: String -> String
removeTrailing xs
  | isSpace $ last xs = removeTrailing $ init xs
  | otherwise = xs

-- Exercise 6.3.2.
removeSpaces :: FilePath -> IO ()
removeSpaces f = do
  (ft,ht) <- openTempFile "" f
  c <- readFile f
  hPutStr ht (unlines . map removeTrailing $ lines c)
  hClose ht
  renameFile ft f

-- Exercise 7.1.1.
checkExists :: FilePath -> IO Handle
checkExists f = do
  e <- doesFileExist f
  if e
    then openFile f ReadMode
    else do
      putStrLn "File does not exist"
      exitFailure

-- Exercise 7.1.2.
fileHead :: IO ()
fileHead = do
  xs <- getArgs
  (n,h) <- case xs of
    [] -> return (10 :: Int, stdin)
    [f] -> do
      h <- checkExists f
      return (10 :: Int, h)
    [n,f] -> do
      h <- checkExists f
      return (read n :: Int, h)
    _ -> do
      putStrLn "Unexpected number of arguments"
      exitFailure
  s <- hGetContents h
  putStr $ unlines . take n $ lines s
  hClose h
  return ()

-- Exercise 7.2.1.
getHandles :: [FilePath] -> IO [Handle]
getHandles [] = return []
getHandles (f:fs) = do
  e <- doesFileExist f
  if e
    then do
      h <- openFile f ReadMode
      r <- getHandles fs
      return (h:r)
    else do
      putStrLn $ "File does not exist " ++ f
      getHandles fs

-- Exercise 7.2.2.
closeHandles :: [Handle] -> IO ()
closeHandles [] = return ()
closeHandles (h:hs) = do
  hClose h
  closeHandles hs

-- Exercise 7.2.3.
concatContent :: [Handle] -> IO String
concatContent [] = return []
concatContent (h:hs) = do
  c <- hGetContents h
  r <- concatContent hs
  return (c ++ r)

-- Exercise 7.2.4.
sortFiles :: IO ()
sortFiles = do
  xs <- getArgs
  hs <- getHandles xs
  c <- concatContent hs
  putStr $ unlines . sort $ lines c
  closeHandles hs
  return ()

-- Exercise 8.1.
randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' g = r : randoms' g'
  where
    (r,g') = random g

-- Exercise 8.2.
randomPositions :: Int -> Int -> Int -> Int -> IO[(Int,Int)]
randomPositions x1 x2 y1 y2 = do
  let
    iX = x2 - x1 + 1
    iY = y2 - y1 + 1
    xs = randoms (mkStdGen 13) :: [Int]
  return $ zipWith (\x y -> (x `mod` iX , y `mod` iY)) xs (tail xs)

----------------
--12th lecture--
----------------

data Person = Person {
   forename :: String,
   surname  :: String,
   sex      :: Sex,
   mother   :: Maybe Person,
   father   :: Maybe Person,
   partner  :: Maybe Person,
   children :: [Person] } deriving (Show,Read,Eq,Ord)

-- Exercise 1.1.
grandfathersPartnerForename :: Person -> Maybe String
grandfathersPartnerForename p = fmap forename (father p >>= father)

-- Exercise 1.2.1.
stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix s l = fmap reverse (stripPrefix (reverse s) (reverse l))

-- Exercise 1.2.2.
removeAffixes :: String -> String -> String -> Maybe String
removeAffixes p s l = stripPrefix p l >>= stripSuffix s

-- Exercise 2.1.
grandfathersPartnerForename2 :: Person -> Maybe String
grandfathersPartnerForename2 p = do
  f1 <- father p
  f2 <- father f1
  return $ forename f2

-- Exercise 2.2.
main5 :: IO ()
main5 =
  let func f = doesFileExist f >>=
                        (\e -> if e then openFile f ReadMode else return stdin)
      ok (f:_) = func f
      ok []    = return stdin
  in fmap (unlines . sort . lines) (getArgs >>= ok >>= hGetContents) >>= putStr
