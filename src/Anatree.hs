module Anatree where

data Tree = Leaf

-- |Creates an empty anagram tree.
empty :: () -> Tree
empty () = Leaf

-- |Number of words stored within anagram tree.
size :: Tree -> Int
size _ = 0

-- |Size of the anagram tree itself, not the words within.
treeSize :: Tree -> Int
treeSize _ = 1
