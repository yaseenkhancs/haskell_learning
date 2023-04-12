module Gold where

import Data.List

data BST a = Empty | Tree a (BST a) (BST a) deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Empty = Nothing
bstLeft (Tree _ x _) = (Just x)

bstRight :: BST a -> Maybe (BST a)
bstRight Empty = Nothing
bstRight (Tree _ _ y) = (Just y)

bstValue :: BST a -> Maybe a
bstValue (Tree a _ _) = Just a

empty :: BST a
empty = Empty

fromList :: Ord a => [a] -> BST a
fromList x = undefined

middleList :: [a] -> a
middleList (x:[]) = x
middleList (x:y:[]) = x
middleList x = middleList (init(tail x))

fromList' :: Ord a => [a] -> BST a -> BST a
fromList' [] mytree = Empty
fromList' list mytree = Tree (middleList list) ((fromList' (filter (< middleList list) list)) mytree) ((fromList' (filter (> middleList list) list)) mytree)

concTree :: Maybe (BST a) -> BST a
concTree (Just x) = x

concVal :: Maybe a -> a
concVal (Just x) = x

insert' :: Ord a => a -> BST a -> BST a
insert' i Empty = Tree i (Empty) (Empty)
insert' i tree@(Tree x left right)
  | i >= (concVal $ bstValue $ tree) = Tree x left (insert' i (concTree $ bstRight tree))
  | otherwise = Tree x (insert' i (concTree $ bstLeft tree)) right
  

singleton :: a -> BST a
singleton x = Tree x Empty Empty

toList :: BST a -> [a]
toList Empty = []
toList (Tree x y z) = toList(y) ++ [x] ++ toList(z)




