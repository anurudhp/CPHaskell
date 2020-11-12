---
layout: blogs
title: Knapsack DP reinvented
created: 13/11/2020
index: 2
---

\[WIP\] Knapsack DP reinvented
==============================

Here is the problem [Money Sums](https://cses.fi/problemset/task/1745/)
that I shared in my [last blog](cp_blog_1.html)

You have $n$ coins, with values $x_1, x_2 \ldots x_n$. You have to find
all possible totals you can form with them.

The Recurrence
--------------

First, let us solve the problem the usual way. We formulate a recurrence
which we solve using Dynamic Programming.

Let $P(i, s)$ be $1$ if it is possible to form a total of $s$ using the
first $i$ coins. i.e. $x_1, x_2 \ldots x_i$. We start with the base
state - with no coins -

$$
\begin{align*}
P(0, 0) & = 1 & s = 0 & \\
        & = 0 & s \ne 0 &
\end{align*}
$$

Here we consider $P(i, s)$ to be a boolean value, with $\cdot$ meaning
`and` and $+$ meaning `or`.

So, we get:

$$
\begin{align*}
P(i, s) & = 1 & s = 0 & \\
        & = 0 & 0 < s < x_i & \\
        & = P(i - 1, s - x_i) + P(i - 1, s) & s \ge x_i &
\end{align*}
$$

Implementation
--------------

Let us start with the basic outline:

``` haskell
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE UnicodeSyntax #-}
import Control.Arrow ((>>>))

main :: IO ()
main = interact $
  words -- split the input into words
    >>> drop 1 -- drop `n`
    >>> map read -- convert to `Int`s
    >>> solve
    >>> map show -- convert `Int`s to `String`s
    >>> showWithLength -- show length and then the list
  where
    showWithLength xs = unlines [show $ length xs, unwords xs]
```

**DP Updates** The first thing we want to write is a simple DP update,
on adding one element. That is the transition from $P(i - 1)$ to $P(i)$
by adding $x_i$ to the set.

``` haskell
update :: [Bool] -> Int -> [Bool]
update dp x = [ p || p' | p <- dp | p' <- dp' ]
  where
    -- shift by x.
    -- dp'[i] = dp[i - x] ; i >= x
    --        = False i < x
    dp' = replicate x False ++ dp
```

You can try running this with a some samples by firing up `gchi`. Load
this file and call the update function.

Finally just apply the updates sequentially on the initial DP state:
`[True, False, False...]`.

``` haskell
solve :: [Int] -> [Int]
solve xs = dpIxsTrue
  where
    -- compute the full final DP
    dp = foldl update (True : repeat False) xs
    -- drop everything after sum, as they are all False
    dpTillSum = take (sum xs + 1) dp
    -- pair with indices
    dpWithIxs = [(i, p) | i <- [0..] | p <- dpTillSum]
    -- only take the True ones
    dpIxsTrue = [ i | (i, p) <- dpWithIxs, p == True]
```
