---
layout: blog
title: Some Basic Problems
created: 27/11/2020
index: 3
---

Some Basic Problems
===================

This week I will be discussing a couple of basic problems.

Let us first pull up some basic imports.

``` haskell
import Control.Arrow ((>>>))
import Data.List (group, sort)
```

[Repetitions](https://cses.fi/problemset/task/1069)
---------------------------------------------------

This problem asks for the longest substring of equal characters.
Usually, we iterate through the string keeping a running counter, and
reset it whenever the character changes.

``` haskell
solveA1 :: String -> Int
solveA1 s = maximum groupLengths
  where
    groups = group s
    groupLengths = map length groups
```

But notice that we can chain the functions and reduce this code.

``` haskell
solveA :: String -> Int
solveA = maximum . map length . group
```

And finally the main function - take the first line as a string and pass
it to solve.

``` haskell
mainA :: IO ()
mainA = interact $ lines >>> head >>> solveA >>> show
```

[GCD on Blackboard](https://atcoder.jp/contests/abc125/tasks/abc125_c)
----------------------------------------------------------------------

There is an array $$A_1, A_2, \ldots A_n$$. You essentially have to
maximize the GCD of the array after removing one element of your choice.

The first observation is that the GCD will be some factor of a number
that remains in the array. Now we know that either $A_1$ or $A_2$ will
remain for sure (we can only remove one element). So we need to check
for all factors of $A_1$ and $A_2$. For a candidate, we have to check if
at least $n - 1$ of the numbers are divisible by it.

First let us write a function to generate factors of a number. For any
number, we only need to check for factors up to its square root. The
others will be the number divided by one of the small factors already
computed.

``` haskell
factors :: Int -> [Int]
factors n = smallFacs ++ (reverse largeFacs) -- reverse to get ascending order
  where
    cands = takeWhile ((<= n) . (^2)) [1..] -- take all x s.t. x^2 <= n
    smallFacs = filter ((==0) . (n `mod`)) cands -- only those x s.t. n `mod` x == 0
    largeFacs = map (n `div`) smallFacs
```

We can compress this in a more haskell-ey way as:

``` haskell
factors' :: Int -> [Int]
factors' n =
  sort .
    concatMap (\x -> [x, n `div` x]) .
      filter ((== 0) . (n `mod`)) .
        takeWhile ((<= n) . (^ 2)) $
          [1 ..]
```

We need to write a function which gives the largest factor of a number
that divides at least $n - 1$ elements in the array.

``` haskell
bestCD :: [Int] -> Int -> Int
bestCD xs = last . filter hasEnough . factors'
  where
    -- True if there is at most one number that is not divisible by f
    hasEnough f = length [ x | x <- xs, x `mod` f /= 0] <= 1
```

And finally the full solution:

``` haskell
solveB :: [Int] -> Int
solveB xs = max (bestCD xs (xs !! 0)) (bestCD xs (xs !! 1))
```

``` haskell
mainB :: IO ()
mainB = interact $ words >>> drop 1 >>> map read >>> solveB >>> show
```

Next Problem
------------

I will try to implement DFS (efficiently) on graphs. I have not found
any good resources for this, apart from the `Data.Graph` library's
implementation.
