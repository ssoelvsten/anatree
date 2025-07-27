-- | The `Tree` type represents a dictionary of words, [`Char`]. The tree is
--   designed specifically such that one can easily query for (sub)anagrams,
--   i.e. words /w/ and /w'/ where /sort w/ and /sort w'/ are equivalent.
--   The time to do most of these operations are mainly dependent on the size of
--   the tree, /t/, and the alphabet, |Σ|, and not the number of words stored,
--   /n/.
module Anatree where
import qualified Data.Set as Set
import Data.List (sort)

-- * Tree Type

-- | Each node in the tree contains a `Data.Set` `Char` with the words that are each
--   others' anagrams. Each `Node` is contains binary choice of whether a
--   certain character is at a certain position in the normalized (sorted) word.
data Tree = Leaf (Set.Set [Char])
          | Node (Set.Set [Char]) Char Tree Tree

-- * Construction

-- | /O/(1) Creates an empty anagram tree.
empty :: Tree
empty = Leaf (Set.empty)

-- | /O/(/|w|/ log /|w|/ + |Σ|) Add word to the set.
insert :: [Char] -> Tree -> Tree
insert w t = insert' (sort w) t
  where insert' []     (Leaf ws)         = Leaf (Set.insert w ws)
        insert' []     (Node ws c t0 t1) = Node (Set.insert w ws) c t0 t1
        insert' (x:xs) (Leaf ws)         = Node ws x empty (insert' xs empty)
        insert' (x:xs) (Node ws c t0 t1) =
          if x < c then Node ws x (Node Set.empty c t0 t1) (insert' xs empty) else
          if x > c then Node ws c (insert' (x:xs) t0)      t1
                   else Node ws c t0                       (insert' xs t1)

-- * Queries

-- | /O/(/|w|/ log /|w|/ + |Σ|) Is the word in the set?
member :: [Char] -> Tree -> Bool
member w t = member' (sort w) t
  where member' []     (Leaf ws)         = Set.member w ws
        member' []     (Node ws _  _ _)  = Set.member w ws
        member' _      (Leaf _)          = False
        member' (x:xs) t'                = let (Node _ c t0 t1) = t'
          in if x < c then member' xs     t' else
             if x > c then member' (x:xs) t0
                      else member' xs     t1

-- | /O/(1) Is this the empty set?
null :: Tree -> Bool
null (Leaf ws) = Set.null ws
null _         = False

-- | /O/(/t/) Number of elements in the set.
size :: Tree -> Int
size (Leaf ws)         =  Set.size ws
size (Node ws _ t0 t1) = (Set.size ws) + (size t0) + (size t1)

-- | /O/(/t/) Size of the tree itself.
treeSize :: Tree -> Int
treeSize (Leaf _)         = 1
treeSize (Node _ _ t0 t1) = 1 + (treeSize t0) + (treeSize t1)

