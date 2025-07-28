-- | The `Tree` type represents a dictionary of words, [/s/], over symbols from
--   some alphabet, Σ. Most operations require that /s/ be an instance of the
--   `Data.Ord` class.
--
--   The tree is designed specifically to make it easy to query for
--   (sub)anagrams, i.e. words /w/ and /w'/ where /sort w/ and /sort w'/ are
--   equivalent. The time to do most of these operations are mainly dependent on
--   the size of the tree, /t/, the alphabet, |Σ|, the output, /m/, and not the
--   number of words stored, /n/.
--
--   This module is intended to be imported qualified. That is, please import it
--   as follows:
--
--   @import qualified Anatree as Anatree@
--
--   Each set of anagrams are stored using `Data.Set`, meaning that no more than
--   @maxBound::Int@ many anagrams may be stored at once.
module Anatree where
import qualified Data.Set as Set
import Data.List (sort)

-- * Tree Type

-- | Each node in the tree contains a `Data.Set` `Char` with the words that are
--   each others' anagrams. Each `Node` contains a binary choice of whether a
--   certain character is at a certain position in the normalized (sorted) word.
data Tree s = Leaf (Set.Set [s])
            | Node (Set.Set [s]) s (Tree s) (Tree s)

-- * Construction

-- | /O/(1) Creates an empty anagram tree.
empty :: Tree s
empty = Leaf (Set.empty)

-- | /O/(/|w|/ log /|w|/ + |Σ|) Add word to the set.
insert :: Ord s => [s] -> Tree s -> Tree s
insert w t = insert' (sort w) t
  where insert' []     (Leaf ws)         = Leaf (Set.insert w ws)
        insert' []     (Node ws c t0 t1) = Node (Set.insert w ws) c t0 t1
        insert' (x:xs) (Leaf ws)         = Node ws x empty (insert' xs empty)
        insert' (x:xs) (Node ws c t0 t1) =
          if x < c then Node ws x (Node Set.empty c t0 t1) (insert' xs empty) else
          if x > c then Node ws c (insert' (x:xs) t0)      t1
                   else Node ws c t0                       (insert' xs t1)

-- * Queries

-- | /O/(/|w|/ log /|w|/ + |Σ|) The anagrams that exists in the set.
anagram :: Ord s => [s] -> Tree s -> Set.Set [s]
anagram w t = anagram' (sort w) t
  where anagram' []     (Leaf ws)        = ws
        anagram' []     (Node ws _ _ _)  = ws
        anagram' _      (Leaf _)         = Set.empty
        anagram' (x:xs) (Node _ c t0 t1) = if x < c then Set.empty else
                                           if x > c then anagram' (x:xs) t0
                                                    else anagram' xs     t1

-- | /O/(/|w|/ log /|w|/ + |Σ|) Is the word in the set?
member :: Ord s => [s] -> Tree s -> Bool
member w t = Set.member w (anagram w t)

-- | /O/(/|w|/ log /|w|/ + |Σ|) Is the word not in the set?
notMember :: Ord s => [s] -> Tree s -> Bool
notMember w t = not (member w t)

-- | /O/(/|w|/ log /|w|/ + m(|Σ| /|w|/)) The subanagrams that exists in the set.
subanagram :: Ord s => [s] -> Tree s -> Set.Set [s]
subanagram w t = subanagram' (sort w) t
  where subanagram' _      (Leaf ws)       = ws
        subanagram' []     (Node ws _ _ _) = ws
        subanagram' (x:xs) t'              = let (Node ws c t0 t1) = t'
          in Set.union ws (if x < c then subanagram' xs t' else
                           if x > c then subanagram' (x:xs) t0
                           else Set.union (subanagram' xs t0) (subanagram' xs t1))

-- | /O/(1) Is this the empty set?
null :: Tree s -> Bool
null (Leaf ws) = Set.null ws
null _         = False

-- | /O/(/t/) Number of elements in the set.
size :: Tree s -> Int
size (Leaf ws)         =  Set.size ws
size (Node ws _ t0 t1) = (Set.size ws) + (size t0) + (size t1)

-- | /O/(/t/) Size of the tree itself.
treeSize :: Tree s -> Int
treeSize (Leaf _)         = 1
treeSize (Node _ _ t0 t1) = 1 + (treeSize t0) + (treeSize t1)

