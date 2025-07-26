module Anatree where
import qualified Data.Set as Set
import Data.List (sort)

data Tree = Leaf (Set.Set [Char])
          | Node (Set.Set [Char]) Char (Tree) (Tree)

-- |Creates an empty anagram tree.
empty :: Tree
empty = Leaf (Set.empty)

-- |Add word `w` to anagram tree.
insert :: [Char] -> Tree -> Tree
insert w t = insert' (sort w) t
  where insert' []     (Leaf ws)         = Leaf (Set.insert w ws)
        insert' []     (Node ws c t0 t1) = Node (Set.insert w ws) c t0 t1
        insert' (x:xs) (Leaf ws)         = Node Set.empty x (Leaf ws) (insert' xs empty)
        insert' (x:xs) t' = let (Node ws c t0 t1) = t'
          in if x < c  then Node Set.empty x t' (insert' xs empty) else
             if x == c then Node ws        c t0 (insert' xs t1)
                       else Node ws        c (insert' (x:xs) t0) t1

-- |Whether the word `w` is in the anagram tree.
member :: [Char] -> Tree -> Bool
member w t = member' (sort w) t
  where member' []     (Leaf ws)         = Set.member w ws
        member' []     (Node ws _  _ _)  = Set.member w ws
        member' _      (Leaf _)          = False
        member' (x:xs) t' = let (Node _ c t0 t1) = t'
          in if x < c  then member' xs     t' else
             if x == c then member' xs     t1
                       else member' (x:xs) t0

-- |Number of ws stored within anagram tree.
size :: Tree -> Int
size (Leaf ws) = Set.size ws
size (Node ws _ t0 t1) = (Set.size ws) + (size t0) + (size t1)

-- |Size of the anagram tree itself, not the words within.
treeSize :: Tree -> Int
treeSize (Leaf _) = 1
treeSize (Node _ _ t0 t1) = 1 + (treeSize t0) + (treeSize t1)

