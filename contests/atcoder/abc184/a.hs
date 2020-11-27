import Control.Arrow ((>>>))

main = interact $ words >>> map read >>> solve >>> show

solve [a, b, c, d] = a * d - b * c
