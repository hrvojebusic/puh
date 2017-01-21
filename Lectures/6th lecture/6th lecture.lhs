University of Zagreb
Faculty of Electrical Engineering and Computing

PROGRAMMING IN HASKELL

Academic Year 2016/2017

LECTURE 6: Recursive functions 2, strictness

v1.0

(c) 2016 Jan Snajder

==============================================================================

> import Data.Char
> import Data.List

== RECAP =====================================================================

Last week we talked about recursion: standard recursion and accumulator-style
recursion. We'll round off today by looking into two other types of recursion:
guarded recursion and corecursion. We'll also look into laziness of Haskell, in
particular its dark and waylaying nature, and how to tame it.

== GUARDED RECURSION ==========================================================

Sometimes, using an accumulator doesn't even make sense to begin with. For
example, if we do structural recursion on a list and modify each element in
turn. Look at the 'incList' function:

> incList1 :: Num a => [a] -> [a]
> incList1 []     = []
> incList1 (x:xs) = x + 1 : incList1 xs

The space complexity of this function is O(1). (The list that is being
constructed is not counted in the space complexity. Only additional memory
allocated during the computation is counted, but here no extra memory is being
allocated.)

We might give it a shot with an accumulator-style version:

> incList2 :: Num a => [a] -> [a]
> incList2 xs = inc xs []
>   where inc []     ys = ys
>         inc (x:xs) ys = inc xs ((x+1):ys)

The problem here is that we can only prepend to the list (to get an O(1)
operation) and thus the accumulated list will be in reverse order. This would
have been OK for 'reverse', but here it's no good. Moreover, accumulator-style
doesn't buy us anything, as the space complexity of the function already was
O(1).

Another example is 'unzip'. We may give it a try with accumulators (two in this
case):

> unzip' :: [(a,b)] -> ([a],[b])
> unzip' zs = unz zs [] []
>   where unz []          xs ys = (xs, ys)
>         unz ((x, y):zs) xs ys = unz zs (x:xs) (y:ys)

But this is again not good for the same reason as above: we end up with lists
in reverse order. We might first reverse the input list, but that would require
two list traversals (one for the reversal and one for unzipping).

Hence in this case too we should resort to "standard" recursion:

> unzip'' :: [(a,b)] -> ([a],[b])
> unzip'' []         = ([], [])
> unzip'' ((x, y):zs) = (x:xs, y:ys)
>   where (xs, ys) = unzip'' zs

The two functions above ('incList' and 'unzip') have one thing in common: we
use recursion to create the output list(s) incrementally. The result is
immediately becoming available and continues to grow as the recursion
progresses. Because Haskell is LAZY, if we choose to consume just the first
part of the result, the recursion will never generate results beyond that point
since these results are not really needed. In other words, the result of the
function can be CONSUMED LAZILY. This is called GUARDED RECURSION. In guarded
recursion, the recursive call occurs within a "data constructor" (cons operator
':' in this case). Because of laziness, the expression will be evaluated up to
the data constructor and the recursive call delayed until needed.

Notice that guarded recursion is not tail recursive. However, there is nothing
left to be done after exiting the recursive call, so space complexity is O(1).
Hence we call such recursion TAIL RECURSION MODULO CONS.

SUMMARY:

Tail recursion reduces space complexity. To achieve it, use:

* Accumulator-style, but only if you need the whole result (e.g., sum, max,
  length, etc.).

* If you don't need the whole result at once but wish to consume it lazily,
  use guarded recursion, which will give you tail recursion modulo cons.

== CORECURSION ===============================================================

Corecursion is "dual" to recursion: instead of decomposing a structure, we
build it up. In RECURSION, each recursive call is applied to a structure that
is smaller than the input structure. Conversely, in CORECURSION, the recursive
call is applied to a larger structure than the input structure and there is no
base case. The structure that we build up can be finite or infinite. Of course,
because of laziness, we will build only as much as needed.

> ones :: [Integer]
> ones = 1 : ones

> cycle' :: a -> [a]
> cycle' x = x : cycle' x

In each step we can use a part of the already constructed structure.

List of natural numbers:

> nats :: [Integer]
> nats = 0 : next nats
>   where next (x:xs) = x + 1 : next xs

A bit more complex: a list of Fibonacci Numbers:

> fibs :: [Integer]
> fibs = 0 : 1 : next fibs
>   where next (x:ys@(y:_)) = (x+y) : next ys

More details here:
http://programmers.stackexchange.com/questions/144274/whats-the-difference-between-recursion-and-corecursion

== STRICTNESS ================================================================

Haskell is a lazy language. It won't evaluate an expression until this is
required.

> e1 = head [1, 2..]

How does that work? Instead of evaluating the complete list, Haskell generates
a so-called THUNK or SUSPENSION -- a pointer to the expression and a data
structure containing all required to evaluate the expression. A thunk will only
be evaluated when required.

Another example of laziness:

  (&&) :: Bool -> Bool -> Bool
  False && _ = False
  True  && x = x

This function does not evaluate its second argument unless the first argument
is 'True'. We say that the function is NON-STRICT in its second argument. This
means that 'False && undefined' will work, but 'undefined && False' won't.
Similarly, function (||) is non-strict in its second argument, so 'True ||
undefined' will work.

A side note:
We've been a bit sloppy here because we equate NON-STRICTNESS with LAZINESS.
Strictness is a concept from lambda calculus. NON-STRICTNESS means an
expression can be reduced to a value, even if some of its parts are undefined.
LAZY EVALUATION is one of the possible ways to implement non-strictness. It
means that we only evaluate expressions when needed, and create thunks in the
meantime. Read more here:
http://www.haskell.org/haskellwiki/Lazy_vs._non-strict

While in most cases we want the evaluation to be lazy, occasionally lazy
evaluation becomes problematic. For example, we absolutely need lazy evaluation
for this to work:

> filterOdd :: [a] -> [a]
> filterOdd xs = [x | (i, x) <- zip [0..] xs, odd i]

Another, perhaps more insightful example. Recall the sum function:

> sumA :: Num a => [a] -> a
> sumA xs = sum xs 0
>   where sum []     s = s             
>         sum (x:xs) s = sum xs (x + s)

> e2 = sumA [0..100]
> e3 = sumA [0..1000000000]

Note that we're using accumulator here, so this is supposed to be space
efficient because its tail recursive. But despite this, we have a problem here
because '(x + s)' won't really be evaluated until it is needed.

Instead, it will build up thunks:

  sumA [0..10] =>
  => sum (0:[1..10]) 0
  => sum (1:[2..10]) (0+0)
  => sum (2:[3..10]) (1+(0+0))
  => sum (3:[4..10]) (2+(1+(0+0)))
  => sum (4:[5..10]) (3+(2+(1+(0+0))))
  => ...

This causes a so-called MEMORY LEAKAGE. The expression builds up in memory,
consuming more and more space, until it finally gets evaluated. The problem is
that the thunks consume more memory than the values they would evaluate to. To
prevent this from happening, we need a way to FORCE the evaluation of '(x + s)'.

There's a function that does that:

  seq :: a -> b -> b

Function 'seq' evaluates its first argument before it returns its second
argument.

For example:

> e4 = let x = undefined in 2
> e5 = let x = undefined in x `seq` 2
> e6 = let x = undefined in x `seq` snd (x, 5)

We can now define a strict version of sumA:

> sumA' :: Num a => [a] -> a
> sumA' xs = sum xs 0
>   where sum []     s = s             
>         sum (x:xs) s = let a = x + s in a `seq` sum xs a
  
> e7 = sumA' [0..1000000000]

We can also define a strict version of the application operator ($):

  ($!) :: (a -> b) -> a -> b
  f $! x = x `seq` f x

'f $! x' will first evaluate the argument 'x', and then apply a function to it.

For example:

> e8 = let x = undefined in const 5 x
> e9 = let x = undefined in const 5 $! x

It's important to understand that 'seq' does not evaluate "too deep". If
expressions have structure, 'seq' will only "scratche the surface".

For example:

> e10 = let x = (undefined, 42) in x `seq` snd x

Here, 'seq' evaluated only the outermost structure (which is the pair
constructor), and did not proceed to evaluate the actual content of the pair.

When this is not enough, we need to make sure that 'seq' is applied recursively
to subexpressions. You can use the 'deepseq' package for this:
http://hackage.haskell.org/package/deepseq

== NEXT ======================================================================

In Haskell, everything's a function. Next, we'll look at HIGHER ORDER FUNCTIONS
(HOF), which are functions that take or return other functions. HOF allow for
functional design patterns, which make our code more structured, more modular,
and more comprehensible.

