#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE TypeFamilies #-}
#endif

--------------------------------------------------------------------------------
-- |
-- Module      :  Anatree
-- Copyright   :  (c) 2025 Steffan Sølvsten
-- License     :  LGPL v3
--
-- Maintainer  :  soelvsten@cs.au.dk
-- Stability   :  experimental
-- Portability :  portable
--
-- The `Tree` type represents a dictionary of words, [/s/], over symbols from
-- some alphabet, Σ. Most operations require that /s/ be an instance of the
-- `Data.Ord` class.
--
-- The tree is designed specifically to make it easy to query for
-- (sub)anagrams, i.e. words /w/ and /w'/ where /sort w/ and /sort w'/ are
-- equivalent. The time to do most of these operations are mainly dependent on
-- the size of the tree (/t/) the alphabet (|Σ|) the output (/m/), and not the
-- number of words stored (/n/).
--
-- This module is intended to be imported qualified. That is, please import it
-- as follows:
--
-- @import qualified Anatree as Anatree@
--
-- Each set of anagrams are stored in a bucket via a `Data.Set`. Hence, no
-- more than @maxBound::Int@ anagram "collisions" can be stored at once.
module Anatree where
import qualified Data.Foldable as Foldable
import qualified Data.List     as List
import qualified Data.Monoid   as Monoid
import qualified Data.Ord      as Ord
import qualified Data.Set      as Set
import qualified Anatree.Util  as Util
#if __GLASGOW_HASKELL__
import qualified GHC.Exts      as GHCExts
#endif

-- * Tree Type

-- | Each node in the tree contains a `Data.Set` /s/ (/s is most often `Char`)
--   with the words that are each others' anagrams. Each `Node` contains a
--   binary choice of whether a certain character exists in the normalized
--   (sorted) word.
data Tree s = Leaf (Set.Set [s])
            | Node (Set.Set [s]) s (Tree s) (Tree s)

type Size   = Int

-- Equality checking by tree traversal and comparing the anagrams stored in
-- each node.
deriving instance Eq s => Eq (Tree s)

-- Conversion between Anagram tree and list
#if __GLASGOW_HASKELL__
instance (Ord s) => GHCExts.IsList (Tree s) where
  type Item (Tree s) = [s]
  fromList = fromList
  toList   = toList
#endif

-- * Construction

-- | /O/(1) Creates an empty anagram tree.
empty :: Tree s
empty = Leaf (Set.empty)

-- | /O/(|/w/| log(|/w/|)) Creates an anagram tree with a single word.
singleton :: Ord s => [s] -> Tree s
singleton = fromSet . Set.singleton

-- | Repeated use of `insert` into an `empty` anagram tree.
fromList :: Ord s => [[s]] -> Tree s
fromList ws = Prelude.foldr insert empty ws

-- | Repeated use of `insert` into an `empty` anagram tree.
fromSet :: Ord s => Set.Set [s] -> Tree s
fromSet ws = Set.foldr insert empty ws

-- * Insertion

-- | /O/(|/w/| log |/w/| + |Σ|) Add word to the set.
insert :: Ord s => [s] -> Tree s -> Tree s
insert w t = insert' (List.sort w) t
  where insert' []     (Leaf ws)         = Leaf (Set.insert w ws)
        insert' []     (Node ws c t0 t1) = Node (Set.insert w ws) c t0 t1
        insert' (x:xs) (Leaf ws)         = Node ws x empty (insert' xs empty)
        insert' (x:xs) (Node ws c t0 t1)
          | x < c     = Node ws x (Node Set.empty c t0 t1) (insert' xs empty)
          | x > c     = Node ws c (insert' (x:xs) t0)      t1
          | otherwise = Node ws c t0                       (insert' xs t1)

-- * Queries

-- | /O/(|/w/| log |/w/| + |Σ|) The anagrams that exists in the set.
anagrams :: Ord s => [s] -> Tree s -> Set.Set [s]
anagrams w t = anagrams' (List.sort w) t
  where anagrams' []     (Leaf ws)        = ws
        anagrams' []     (Node ws _ _ _)  = ws
        anagrams' _      (Leaf _)         = Set.empty
        anagrams' (x:xs) (Node _ c t0 t1)
          | x < c     = Set.empty
          | x > c     = anagrams' (x:xs) t0
          | otherwise = anagrams' xs     t1

-- | /O/(|/w/| log |/w/| + |Σ|) Is the word in the set?
member :: Ord s => [s] -> Tree s -> Bool
member w t = Set.member w (anagrams w t)

-- | /O/(|/w/| log |/w/| + |Σ|) Is the word not in the set?
notMember :: Ord s => [s] -> Tree s -> Bool
notMember w t = not (member w t)

-- | /O/(|/w/| log |/w/| + m(|Σ| |/w/|)) The subanagrams that exists in the set.
subanagrams :: Ord s => [s] -> Tree s -> Set.Set [s]
subanagrams w t = subanagrams' (List.sort w) t
  where subanagrams' _         (Leaf ws)         = ws
        subanagrams' []        (Node ws _ _  _)  = ws
        subanagrams' (x:xs) t'@(Node ws c t0 t1)
          | x < c     = subanagrams' xs t'
          | x > c     = Set.union ws (subanagrams' (x:xs) t0)
          | otherwise = Set.union ws $ Set.union (subanagrams' xs t0) (subanagrams' xs t1)

-- | /O/(/n²/) The words of a certain length that are not anagrams of each other.
keys :: Ord s => Int -> Tree s -> Set.Set [s]
keys 0 (Leaf ws)         = Set.take 1 ws
keys 0 (Node ws _ t0 _ ) = Set.union (Set.take 1 ws) (keys 0 t0)
keys _ (Leaf _)          = Set.empty
keys k (Node _  _ t0 t1) = Set.union (keys k t0) (keys (k-1) t1)

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

-- * Folds

-- | Fold the words in the set using a right-associative binary operator.
foldr :: ([s] -> t -> t) -> t -> Tree s -> t
foldr f x (Leaf ws)         = Set.foldr f x ws
foldr f x (Node ws _ t0 t1) = let x'  = Anatree.foldr f x  t1
                                  x'' = Anatree.foldr f x' t0
                              in Set.foldr f x'' ws

-- | Fold the words in the set using a left-associative binary operator.
foldl :: (t -> [s] -> t) -> t -> Tree s -> t
foldl f x (Leaf ws)         = Set.foldl f x ws
foldl f x (Node ws _ t0 t1) = let x'  = Set.foldl f x ws
                                  x'' = Anatree.foldl f x' t0
                              in Anatree.foldl f x'' t1

-- | Fold mapped words in the set.
foldMap :: Monoid.Monoid m => ([s] -> m) -> Tree s -> m
foldMap f (Leaf ws)         = Foldable.foldMap f ws
foldMap f (Node ws _ t0 t1) =
 (Foldable.foldMap f ws) `mappend` (Anatree.foldMap f t0) `mappend` (Anatree.foldMap f t1)

-- * Conversion
-- ** List

-- | An alias of `toList`.
elems :: Tree s -> [[s]]
elems = toList

-- | Convert the set to a list of elements.
toList :: Tree s -> [[s]]
toList t = Anatree.foldr (:) [] t

-- | Convert the set into an ascending list of elements.
toAscList :: Ord s => Tree s -> [[s]]
toAscList (Leaf ws)         = Set.toAscList ws
toAscList (Node ws _ t0 t1) = merge (Set.toAscList ws)
                            $ merge (toAscList t0) (toAscList t1)
  where merge = Util.mergeBy (Ord.compare)

-- | Convert the set into a descending list of elements.
toDescList :: Ord s => Tree s -> [[s]]
toDescList (Leaf ws) = Set.toDescList ws
toDescList (Node ws _ t0 t1) = merge (Set.toDescList ws)
                             $ merge (toDescList t0) (toDescList t1)
  where merge = Util.mergeBy (Ord.comparing Ord.Down)

-- ** Set

-- | Convert the set of words to a `Set`.
toSet :: Ord s => Tree s -> Set.Set [s]
toSet t = Anatree.foldr Set.insert Set.empty t
