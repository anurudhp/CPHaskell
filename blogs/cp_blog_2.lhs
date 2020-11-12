---
layout: blogs
title: Knapsack DP reinvented
created: 13/11/2020
index: 2
---

Knapsack DP reinvented
======================

Here is the problem [Money Sums](https://cses.fi/problemset/task/1745/) that I shared in my [last blog](cp_blog_1.html)

You have $n$ coins, with values $x_1, x_2 \ldots x_n$. You have to find all possible totals you can form with them.

The Recurrence
--------------
First, let us solve the problem the usual way. We formulate a recurrence which we solve using Dynamic Programming.

Let $P(i, s)$ be $1$ if it is possible to form a total of $s$ using the first $i$ coins. i.e. $x_1, x_2 \ldots x_i$.
We start with the base state - with no coins -

$$
\begin{align*}
P(0, 0) & = 1 & s = 0 & \\
        & = 0 & s \ne 0 &
\end{align*}
$$

Here we consider $P(i, s)$ to be a boolean value, with $\cdot$ meaning `and` and $+$ meaning `or`.

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
\begin{code}
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
\end{code}

**DP Updates**
The first thing we want to write is a simple DP update, on adding one element. That is the transition from $P(i - 1)$ to $P(i)$ by adding $x_i$ to the set.

\begin{code}
update :: [Bool] -> Int -> [Bool]
update dp x = [ p || p' | p <- dp | p' <- dp' ]
  where
    -- shift by x to get the updating list: dp'
    -- and then combine it with the old state.
    -- dp'[i] = dp[i - x] ; i >= x
    --        = False i < x
    dp' = replicate x False ++ dp
\end{code}

You can try running this with a some samples by firing up `gchi`. Load this file and call the update function. Remember that `dp` is an infinite list. You can use `take <n>` to see the first `n` elements of it.

Finally just apply the updates sequentially on the initial DP state: `[True, False, False...]`.
\begin{code}
solve :: [Int] -> [Int]
solve xs = dpIxsTrue
  where
    -- compute the full final DP
    dp = foldl update (True : repeat False) xs
    -- pair with indices, upto the total
    dpWithIxs = [(i, p) | i <- [0..sum xs] | p <- dp]
    -- only take the True ones, excluding 0
    dpIxsTrue = [ i | (i, p) <- dpWithIxs, p == True, i > 0]
\end{code}

A very nice use of laziness. We just took the mathematical definition of the DP, and let it run till infinity. This also gives us a clean implementation, which is intuitive to understand.

Here is a link to my [submission](https://github.com/anurudhp/CPHaskell/blob/master/contests/cses/1745.hs). I have written two variants for solve there, one same as above, and one without using infinite lists.

Next Problem
------------
TBA
