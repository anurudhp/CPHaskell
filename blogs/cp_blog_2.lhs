---
layout: blog
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
P(0, s) \equiv
\begin{cases}
\texttt{true} & s = 0 \\
\texttt{false} & s > 0 \\
\end{cases}
$$

And here is the recurrence for using the first $i$ coins.
$$
P(i, s) \equiv
\begin{cases}
\texttt{true} & s = 0 \\
\texttt{false} & 0 < s < x_i \\
P(i - 1, s - x_i) \lor P(i-1, s) & s \geq x_i \\
\end{cases}
$$

For simplicity, let us define another family of sequences - $D$.
$$D_i = \{ P(i, 0), P(i, 1), P(i, 2), \ldots \}$$

So we start with $D_0$ and to get $D_i$ from $D_{i - 1}$, we add the coin $x_i$.

Implementation
--------------

Let us start with the basic outline:
\begin{code}
{-# LANGUAGE ParallelListComp #-}
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

 ### Language Extension: `ParallelListComp`

I will write my solutions here using some syntactic sugar for zip. This just makes it easier to read.

Here is an example.
\begin{code}
zipAdd' :: [Int] -> [Int] -> [Int]
zipAdd' xs ys = [ x + y | x <- xs | y <- ys ]
\end{code}
You can add mutiple lists, separated by a pipe symbol. This is equivalent to zipping all of them, with the expression at the start.

 ### Transition

We want a function that computes $D_i$ from $D_{i - 1}$, by adding $x_i$ to the set. Let us call $D_{i - 1}$ as `dp`, and the new value $D_i$ as `dp'`. This implies that `dp' = next dp x`.

To compute `dp'[i]`, we need `dp[i]` and `dp[i - x]`. Define `dpx[i] = dp[i - x]` by just prepending `False` `x` times to `dp`.

Now we get `dp'[i] = dp[i] || dpx[i]`. This is computed by directly zipping them with `||`.

\begin{code}
next :: [Bool] -> Int -> [Bool]
next dp x = [ p || p' | p <- dp | p' <- dpx ]
  where
    dpx = replicate x False ++ dp
\end{code}

You can try running this with a some samples by firing up `gchi`. Load this file and call the `next` function. Remember that `dp` is an infinite list. You can use `take n` to see the first `n` elements of it.

```haskell
ghci> dp0 = True : repeat False
ghci> dp1 = next dp0 3
ghci> dp2 = next dp1 4
ghci> take 8 dp2
[True,False,False,True,True,False,False,True]
ghci> take 8 (zip [0..] dp2)
[(0,True),(1,False),(2,False),(3,True),(4,True),(5,False),(6,False),(7,True)]
```

 ### Final solution

Finally just apply the updates sequentially on the initial DP state: $$D_0 = \{True, False, False, \ldots\}$$.

\begin{code}
solve :: [Int] -> [Int]
solve xs = dpIxsTrue
  where
    -- D_0
    dp0 = True : repeat False
    -- compute the full final DP
    dp = foldl next dp0 xs
    -- pair with indices, upto the total
    dpWithIxs = [(i, p) | i <- [0..sum xs] | p <- dp]
    -- only take the True ones, excluding 0
    dpIxsTrue = [ i | (i, p) <- dpWithIxs, p == True, i > 0]
\end{code}

A very nice use of laziness. We just took the mathematical definition of the DP, and let it run till infinity. This also gives us a clean implementation, which is intuitive to understand.

Here is a link to my final [submission](https://github.com/anurudhp/CPHaskell/blob/master/contests/cses/1745.hs). I have written two variants for solve there, one same as above, and one without using infinite lists.

Next Problem
------------
TBA
