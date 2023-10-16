module Ex_foldl where

import Data.List
import Data.Char

--length

lengthl :: [a] -> Int
lengthl x = foldl f 0 x
  where f x y = x+1

concatl :: [a] -> [a] -> [a]
concatl a b = foldl f a b
  where f x y = x++[y]

productl :: Num a => [a] -> a
productl a = foldl f 1 a
  where f x y = x * y

orl :: [Bool] -> Bool
orl a = foldl f False a
  where f x y = x || y

lastl :: [a] -> Maybe a
lastl a = foldl f Nothing a
  where f x y = Just y

firstl :: [a] -> Maybe a
firstl a = foldl f Nothing a
  where f Nothing y = Just y
        f (Just x) y = (Just x)

reversel :: [a] -> [a]
reversel a = foldl f [] a
  where f x y = [y] ++ x

filterl :: (a -> Bool) -> [a] -> [a]
filterl cond a = foldl f [] a
  where f x y
          | cond y = x ++ [y]
          | otherwise = x