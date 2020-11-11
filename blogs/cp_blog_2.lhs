---
layout: blogs
title: Knapsack DP reinvented 
created: 13/11/2020
index: 2
---

[WIP] Knapsack DP reinvented
==================================

Here is the problem [Money Sums](https://cses.fi/problemset/task/1745/) that I shared in my [last blog](cp_blog_1.html)

You have $n$ coins, with values $x_1, x_2 \ldots x_n$. You have to find all possible totals you can form with them.

The Recurrence
--------------
First, let us solve the problem the usual way. We formulate a recurrence which we solve using Dynamic Programming.

Let $P(i, s)$ be $1$ if it is possible to form a total of $s$ using the first $i$ coins. i.e. $x_1, x_2 \ldots x_i$.
We start with the base state - with no coins -

\[
\begin{align*}
P(0, s) & = 1 & s = 0
 & = 2 & s = 1
\end{align*}
\]

Implementation
--------------

Let's start with the basic outline:
\begin{code}
import Control.Arrow ((>>>))

main :: IO ()
main =
  interact $
    words
    >>> drop 1
    >>> map read
    >>> solve
    >>> map show
    >>> showWithLength
  where
    showWithLength xs = unlines [show $ length xs, unwords xs]

solve :: [Int] -> [Int]
solve = error "not implemented"
\end{code}

