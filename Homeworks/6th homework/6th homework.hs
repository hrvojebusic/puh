module Homework6 where

-- Problem 3

-- Defines types that can be interpreted as boolean values.
class Truthy a where
  truey :: a -> Bool
  falsey :: a -> Bool

  -- Minimal definition.
  truey = not . falsey
  falsey = not . truey

-- Declaring Bool as instance of Truthy.
instance Truthy Bool where
  falsey True = False
  falsey False = True

-- Declaring Int as instance of Truthy.
instance Truthy Int where
  falsey 0 = True
  falsey _ = False

-- Declaring [a] as instance of Truthy.
instance Truthy [a] where
  falsey [] = True
  falsey _ = False

-- Function that works on instances of Truthy and behaves like the
-- if-then-else construct.
if' :: Truthy p => p -> a -> a -> a
if' truthy ok notOk = if truey truthy then ok else notOk

-- Takes a Truthy value and another argument. If the first argument
-- evaluates to truey, it returns the second argument. Otherwise
-- it raises an error.
assert :: Truthy p => p -> a -> a
assert t a = if' t a (error "Assertion failed")

-- Function that behaves like && function but operates on Truthy
-- instances.
(&&&) :: (Truthy a, Truthy b) => a -> b -> Bool
(&&&) t1 t2 = truey t1 && truey t2

-- Function that behaves like || function but operates on Truthy
-- instances.
(|||) :: (Truthy a, Truthy b) => a -> b -> Bool
(|||) t1 t2 = truey t1 || truey t2

-- Problem 4

-- Datatype DiffList holds a concatenation computation - given
-- a list it will hold a function - the concatenation of that
-- list and another lsit it expects as an argument.
data DiffList a = DiffList { undiff :: [a] -> [a] }

-- Function constructs an empty DiffList.
empty :: DiffList a
empty = DiffList { undiff = ([] ++) }

-- Function that takes a list and returns a DiffList as its
-- concatenation computation.
fromList :: [a] -> DiffList a
fromList xs = DiffList { undiff = (xs ++) }

-- Function that takes a DiffList and returns its computation,
-- a concatenated list.
toList :: DiffList a -> [a]
toList dl = undiff dl []

-- Function that takes two DiffLists and combines them into a
-- new DiffList.
append :: DiffList a -> DiffList a -> DiffList a
append dl1 dl2 = DiffList { undiff = undiff dl1 . undiff dl2 }

-- Declaring DiffList as instance of Monoid.
instance Monoid (DiffList a) where
  mempty = empty
  mappend = append
