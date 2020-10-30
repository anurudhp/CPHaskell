---
layout: blogs
title: Introduction and out first problem
created: 30/10/2020
index: 1
---

Introduction and our first problem
==================================

Basic Setup
-----------

My basic setup is heavily inspired by [Brent
Yorgey](https://byorgey.wordpress.com/). First have a look at [this
blog](https://byorgey.wordpress.com/2019/04/24/competitive-programming-in-haskell-basic-setup/)
for the basic template we'll be using.

``` haskell
import Control.Arrow -- for >>>
```

``` {.haskell}
main :: IO ()
main = interact $ _ 
```

There are also a few basic problems at the end of the above blog. I
recommend you first try to solve them before attempting the problem that
I am presenting.

Literate Haskell
----------------

This entire blog post is a haskell file! You can find the link at the
bottom, named *literate haskell source*. You can download the file and
run it directy using `runhaskell`.

Warmup
------

Let us solve an easy problem to get started.

**[AtCoder: Resale](https://atcoder.jp/contests/abc125/tasks/abc125_b)**

This is the **B** problem from an AtCoder beginner contest.

You have $n$ gems, with values $V_i$ and cost $C_i$. You want to pick
some such that total value minus total cost is maximized. So you
basically pick the ones which have $V_i > C_i$.

Let us parse the input.

``` haskell
main = interact process
process :: String -> String
process =
  lines {- convert to a list of lines -}
    >>> drop 1 {- first line contains N, we don't need it -}
    >>> map words {- words splits a string into words. we run that over each line -}
    >>> map (map read) {- read parses a string to any type -}
    >>> solve
    >>> show {- convert to string -}

-- Given a list of two lists, take pairwise differences, and add up the positive ones.
solve1 :: [[Int]] -> Int
solve1 [vs, cs] = sum contribs
  where
    pairs = zip vs cs {- zip [1,2,3] [4,5,6] = [(1,4), (2,5), (3,6)] -}
    contrib (v, c) = max 0 (v - c)
    contribs = map contrib pairs {- apply contrib on each pair -}
```

Let us try to simplify this, by rewriting a `zip` followed by a `map`
using a `zipWith`

``` {.haskell}
-- f :: (a, b) -> c
-- f' :: a -> b -> c -- curried version of f 
map f (zip xs ys) = zipWith f' xs ys
```

``` haskell
solve2 [vs, cs] = sum contribs
  where
    contrib' v c = max 0 (v - c)
    contribs = zipWith contrib' vs cs
```

We can further simplify this by first computing the differences, then
filtering out the negative ones.

``` haskell
solve3 [vs, cs] = sum contribs
  where
    contrib' v c = v - c
    contribs = filter (> 0) (zipWith contrib' vs cs)
```

Notice that `contrib'` is exactly the subtraction operator `(-)`!

``` haskell
solve4 [vs, cs] = sum contribs
  where
    contribs = filter (> 0) (zipWith (-) vs cs)
```

Finally, the shortest code I could write:

``` haskell
solve [vs, cs] = sum . filter (> 0) $ zipWith (-) vs cs
```

How slick is that!

Next Problem
------------

Here's the first problem I'll be discussing: [Money
Sums](https://cses.fi/problemset/task/1745/) from
[cses.fi](https://cses.fi/). It is a very standard knapsack problem, but
try to solve it using haskell. I'll post the solution next Friday which
is 6th November, 2020.

Feel free to discuss in the disqus comments at the end of this page.
