module Foldr_ex where

import Data.List
import Data.Char 

--length of a finite list

flength :: [a] -> Int
flength x = foldr f 0 x
  where f x y = y+1

--(++)

fconcat :: [a] -> [a] -> [a]
fconcat x y = foldr f y x
  where f x y = x:y

--product of finite list of numbers

fproduct :: Num a => [a] -> a
fproduct x = foldr f 1 x
  where f x y = x*y

--or, disjunction of boolean list

foldor :: [Bool] -> Bool
foldor x = foldr f False x
  where f True y = True
        f _ y = y

--any, see if any meets the predicate

foldany :: (a -> Bool) -> [a] -> [a]
foldany predic x = foldr f [] x
  where f x y
          | predic x = x:y
          | otherwise = y

foldany' :: (a -> Bool) -> [a] -> Bool
foldany' predic x = foldr f False x
  where f x y = (predic x) || y

foldall :: (a -> Bool) -> [a] -> Bool
foldall predic x = foldr f True x
  where f x y = (predic x) && y

foldnmap :: (a -> b) -> [a] -> [b]
foldnmap j x = foldr f [] x
  where f x y = j x : y

freverse :: [a] -> [a]
freverse x = foldr f [] x
  where f x y = y ++ [x]

fflatten :: [[a]] -> [a]
fflatten xs = foldr f [] xs
  where f x y = x ++ y

maybelast :: [a] -> Maybe a
maybelast a = foldr f Nothing a
  where f x Nothing = (Just x)
        f x y = y

fnub :: Eq a => [a] -> [a]
fnub x = foldr f [] x
  where f x y
          | x `elem` y = y
          | otherwise = x:y