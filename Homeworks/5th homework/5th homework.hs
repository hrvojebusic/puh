module Homework5 where

import           Data.List
import           Text.Read
import           Text.Regex.Posix

-- Problem 1
-- x and y coordinates
type Position = (Integer, Integer)
data Orientation = West | North | East | South deriving (Enum, Bounded, Eq, Show)
-- Clockwise and Counterclockwise
data TurnDir = CW | CCW deriving (Eq, Show)

data Turtle = Turtle
  { position :: Position
  , orientation :: Orientation  } deriving Show

-- Creates a Turtle at Position (0,0) facing upwards.
newTurtle :: Turtle
newTurtle = Turtle { position = (0,0) , orientation = North }

{-
  Takes an Integer and a Turtle and moves the turtle a given amount in the
  direction it is currently facing.
-}
move :: Integer -> Turtle -> Turtle
move n t@Turtle { position = (x,y), orientation = o' }
  | n < 0 = error "Turtles cannot move backwards"
  | o' == North = t { position = (x, y + n) }
  | o' == South = t { position = (x, y - n) }
  | o' == East = t { position = (x + n, y)  }
  | otherwise = t { position = (x - n, y) }

-- Helper functions to help cycle through all Orientations.
-- Retrieves enumeration which 'succeedes' given one.
next :: (Enum a, Bounded a) => a -> a
next = getEnum 1

-- Retrieves enumeration which 'preceeds' given one.
prev :: (Enum a, Bounded a) => a -> a
prev = getEnum (-1)

-- Specifies which enumeration should be returned according to cycling direction.
getEnum :: (Enum a, Bounded a) => Int -> a -> a
getEnum n e = toEnum (add (fromEnum (maxBound `asTypeOf` e) + 1) (fromEnum e) n)
    where
      add size x y = (x + y + size) `rem` size

-- Retrieves next orientation for given turn direction.
getOrientation :: Orientation -> TurnDir -> Orientation
getOrientation o d
  | d == CW = next o
  | otherwise =  prev o

-- Takes a TurnDir and a Turtle and changes the turtle's position accordingly.
turn :: TurnDir -> Turtle -> Turtle
turn d t@Turtle { orientation = o } = t { orientation = getOrientation o d }

-- Enables us to chain our commands to the turtle more easily.
runTurtle :: [Turtle -> Turtle] -> Turtle -> Turtle
runTurtle fs t = foldl (\acc f -> f acc) t fs

-- Problem 2
-- Tree data type.
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Eq, Show)

{-
  Function that takes a predicate and a Tree and removes those subtrees
  that do not satisfy the given predicate (with any children).
-}
treeFilter :: (a -> Bool) -> Tree a -> Tree a
treeFilter _ s@Leaf = s
treeFilter f (Node a t1 t2)
  | f a = Node a (treeFilter f t1) (treeFilter f t2)
  | otherwise = Leaf

{-
  Function that takes some binary function and applies it to the tree.
  The function that is being applied takes the depth of the tree as the
  first argument. The root element’s depth is 0.
-}
levelMap :: (Int -> a -> b) -> Tree a -> Tree b
levelMap = levelMap' 0
  where
    levelMap' :: Int -> (Int -> a -> b) -> Tree a -> Tree b
    levelMap' _ _ Leaf = Leaf
    levelMap' lvl f (Node a t1 t2) =
       Node (f lvl a) (levelMap' (lvl+1) f t1) (levelMap' (lvl+1) f t2)

{-
 Function that takes two instances of Tree and checks whether
 the first tree appears as part of the second.
-}
isSubtree :: Eq a => Tree a -> Tree a -> Bool
isSubtree Leaf Leaf = True
isSubtree Node{} Leaf = False
isSubtree t t'@(Node _ t1 t2)
 | areEqual t t' = True
 | otherwise = isSubtree t t1 || isSubtree t t2

-- Function that checks whether two instances of Tree are equal.
areEqual :: Eq a => Tree a -> Tree a -> Bool
areEqual Leaf Leaf = True
areEqual (Node a1 t11 t12) (Node a2 t21 t22)
  | a1 == a2 = areEqual t11 t21 && areEqual t12 t22
  | otherwise = False
areEqual _ _ = False

-- Problem 3
-- Represents a boolean value.
type Val = Bool
-- Represents a boolean expression.
data Pred = Val Val | And Pred Pred | Or Pred Pred | Not Pred

-- Function that takes a Pred and returns it's evaluated Bool value.
eval :: Pred -> Bool
eval (And p1 p2) = eval p1 && eval p2
eval (Or p1 p2)  = eval p1 || eval p2
eval (Not p1)    = not $ eval p1
eval (Val e)     = e

-- Problem 4
-- Sorts tracks by track number.
sortTracks :: [String] -> [String]
sortTracks  = map snd . sort . map (\t -> (getTrackNumber' t, t))

-- Retrieves track number.
{-
  DISCLAIMER: This implementation takes first 'interpretable' String
  as track number, which leaves room for error if track name includes
  number. This implementation presents the most simple solution. Another
  implementation is provided at the end by utilizing regular expressions.
-}
getTrackNumber :: String -> Int
getTrackNumber xs = read (head $ filter isNumber' (words xs)) :: Int

-- Calculates the number of played tracks for a whole album.
numberOfPlays :: [String] -> Int
numberOfPlays = foldl (\acc t -> acc + getTrackPlays t) 0

-- Retrieves number of plays for single track - value that is prepended.
getTrackPlays :: String -> Int
getTrackPlays xs = read (head $ words xs) :: Int

-- Tells whether String can be interpreted as Integer.
isNumber' :: String -> Bool
isNumber' xs =
  case readMaybe xs :: Maybe Int of
    Just _  -> True
    Nothing -> False

-- Solution for track number with regex, left it here for future considerations.
pattern' :: String
pattern' = "[a-zA-Z\\s]+ ([0-9][0-9]) [a-zA-Z\\s]+"

getTrackNumber' :: String -> Int
getTrackNumber' xs
  | length matched == 1 && length (head matched) == 2 = read number :: Int
  | otherwise = error "Can not extract track number"
  where
    matched :: [[String]]
    matched = xs =~ pattern' :: [[String]]
    number :: String
    number = head matched !! 1

-- Problem 5
-- Possible confusing tuples that could occur in ambiguous message.
confusions :: [String]
confusions = ["NW","NE","SW","SE"]

{-
  Calculates how many possible routes are there in a message that doesn’t
  have commas.
-}
possibleRoutes :: String -> Int
possibleRoutes xs = 2 ^ numberOfConfusions
  where
    numberOfConfusions :: Int
    numberOfConfusions =
      foldl (\acc w -> if w `elem` confusions then acc + 1 else acc) 0 $ map (\t -> [fst t,snd t]) (zip xs $ tail xs)
