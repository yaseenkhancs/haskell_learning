module Ex_listcomps where

import Data.List
import Data.Char

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

kek = length [(x, y) | x <- mySqr, y <- myCube, x<50, y<50]

--sum of squares

sumsq = [x^2 | x <- [1..100]]

--gridfunc

grid :: Int -> Int -> [(Int, Int)]
grid i j = [(x, y) | x <- [0..i], y <- [0..j]]

--squaregrid

sqgr :: Int -> [(Int, Int)]
sqgr i = [(x, y) | x <- [0..i], y <- [0..i], x/=y]

--replicate

lcrepl :: Int -> a -> [a]
lcrepl i a = [a | x <- [1..i]]

--pythagorean triple

pyths :: Int -> [(Int, Int, Int)]
pyths i = [(a, b, c) | a <- [1..i], b <- [1..i], c <- [1..i], a^2 + b^2 == c^2]

--perfect positive integer

factors :: Int -> [Int]
factors i = factors' (i-1) i

factors' 0 i = []
factors' i main
  | main `mod` i == 0 = (factors' (i-1) main) ++ [i]
  | otherwise = factors' (i-1) main

perfects :: Int -> [Int]
perfects i = [x | x <- [1..i], (sum $ factors x) == x]