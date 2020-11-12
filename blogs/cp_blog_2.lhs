---
layout: blogs
title: Knapsack DP reinvented
created: 13/11/2020
index: 2
---

[WIP] Knapsack DP reinvented
============================

Here is the problem [Money Sums](https://cses.fi/problemset/task/1745/) that I shared in my [last blog](cp_blog_1.html)

You have $n$ coins, with values $x_1, x_2 \ldots x_n$. You have to find all possible totals you can form with them.

The Recurrence
--------------
First, let us solve the problem the usual way. We formulate a recurrence which we solve using Dynamic Programming.

Let $P(i, s)$ be $1$ if it is possible to form a total of $s$ using the first $i$ coins. i.e. $x_1, x_2 \ldots x_i$.
We start with the base state - with no coins -

$$
\begin{align*}
P(0, 0) & = 1 \\
P(0, s) & = 0 & s \ne 0
\end{align*}
$$

Here, we consider $P(i, s)$ to be a boolean value, with $\cdot$ meaning \texttt{and} and $+$ meaning \texttt{or}.

So, we get:

$$
\begin{align*}
P(i, s) & = 1 & s = 0 \\
        & = 0 & s < x_i \\
        & = P(i - 1, s - x_i) + P(i - 1, s) & s >= x_i
\end{align*}
$$

Implementation
--------------

Let us start with the basic outline:
\begin{code}
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

solve :: [Int] -> [Int]
solve = error "not implemented"
\end{code}

