module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Data.Set as Set
import qualified Anatree as Anatree

-- Tests with the empty tree [2]
tests_Empty :: Test.HUnit.Test
tests_Empty = let t = Anatree.fromList [] :: Anatree.Tree Char
  in "[]" ~: Test.HUnit.TestList [
  "size"            ~: 0     ~=? (Anatree.size t),
  "null"            ~: True  ~=? (Anatree.null t),
  "treeSize"        ~: 1     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "keys 0"          ~: (Set.fromList []) ~=? (Anatree.keys 0 t),
  "member 'a'"      ~: False ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: True  ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: False ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: True  ~=? (Anatree.notMember "b" t),
  "keys 1"          ~: (Set.fromList []) ~=? (Anatree.keys 1 t),
  "member 'ab'"     ~: False ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList []) ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ab'" ~: (Set.fromList []) ~=? (Anatree.subanagrams "ab" t),
  "member 'ba'"     ~: False ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList []) ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ba'" ~: (Set.fromList []) ~=? (Anatree.subanagrams "ba" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t),
  "anagram 'ac'"    ~: (Set.fromList []) ~=? (Anatree.anagrams "ac" t),
  "subanagram 'ac'" ~: (Set.fromList []) ~=? (Anatree.subanagrams "ac" t),
  "keys 2"          ~: (Set.fromList []) ~=? (Anatree.keys 2 t)
  ]

tests_Eta :: Test.HUnit.Test
tests_Eta = let t = Anatree.fromList [""]
  in "['']" ~: Test.HUnit.TestList [
  "size"            ~: 1     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 1     ~=? (Anatree.treeSize t),
  "member ''"       ~: True  ~=? (Anatree.member "" t),
  "notMember ''"    ~: False ~=? (Anatree.notMember "" t),
  "keys 0"          ~: (Set.fromList [""]) ~=? (Anatree.keys 0 t),
  "member 'a'"      ~: False ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: True  ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: False ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: True  ~=? (Anatree.notMember "b" t),
  "keys 1"          ~: (Set.fromList []) ~=? (Anatree.keys 1 t),
  "member 'ab'"     ~: False ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList [])   ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ab'" ~: (Set.fromList [""]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ba'"     ~: False ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList [])   ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ba'" ~: (Set.fromList [""]) ~=? (Anatree.subanagrams "ba" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t),
  "anagram 'ac'"    ~: (Set.fromList [])   ~=? (Anatree.anagrams "ac" t),
  "subanagram 'ac'" ~: (Set.fromList [""]) ~=? (Anatree.subanagrams "ac" t),
  "keys 2"          ~: (Set.fromList []) ~=? (Anatree.keys 2 t)
  ]

-- Tests with a single 'a' node [2]
tests_A :: Test.HUnit.Test
tests_A = let t = Anatree.fromList ["a"]
  in "['a']" ~: Test.HUnit.TestList [
  "size"            ~: 1     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 3     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "keys 0"          ~: (Set.fromList []) ~=? (Anatree.keys 0 t),
  "member 'a'"      ~: True  ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: False ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: False ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: True  ~=? (Anatree.notMember "b" t),
  "keys 1"          ~: (Set.fromList ["a"]) ~=? (Anatree.keys 1 t),
  "member 'ab'"     ~: False ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList [])    ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ab'" ~: (Set.fromList ["a"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ba'"     ~: False ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList [])    ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ba'" ~: (Set.fromList ["a"]) ~=? (Anatree.subanagrams "ba" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t),
  "anagram 'ac'"    ~: (Set.fromList [])    ~=? (Anatree.anagrams "ac" t),
  "subanagram 'ac'" ~: (Set.fromList ["a"]) ~=? (Anatree.subanagrams "ac" t),
  "keys 2"          ~: (Set.fromList []) ~=? (Anatree.keys 2 t)
  ]

tests_A_A :: Test.HUnit.Test
tests_A_A = let t = Anatree.fromList ["a", "a"]
  in "['a','a']" ~: Test.HUnit.TestList [
  "size"            ~: 1     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 3     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "keys 0"          ~: (Set.fromList []) ~=? (Anatree.keys 0 t),
  "member 'a'"      ~: True  ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: False ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: False ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: True  ~=? (Anatree.notMember "b" t),
  "keys 1"          ~: (Set.fromList ["a"]) ~=? (Anatree.keys 1 t),
  "member 'ab'"     ~: False ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList [])    ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ab'" ~: (Set.fromList ["a"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ba'"     ~: False ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList [])    ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ba'" ~: (Set.fromList ["a"]) ~=? (Anatree.subanagrams "ba" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t),
  "anagram 'ac'"    ~: (Set.fromList [])    ~=? (Anatree.anagrams "ac" t),
  "subanagram 'ac'" ~: (Set.fromList ["a"]) ~=? (Anatree.subanagrams "ac" t),
  "keys 2"          ~: (Set.fromList []) ~=? (Anatree.keys 2 t)
  ]

tests_Eta_A :: Test.HUnit.Test
tests_Eta_A = let t = Anatree.fromList ["", "a"]
  in "['','a']" ~: Test.HUnit.TestList [
  "size"            ~: 2     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 3     ~=? (Anatree.treeSize t),
  "member ''"       ~: True  ~=? (Anatree.member "" t),
  "notMember ''"    ~: False ~=? (Anatree.notMember "" t),
  "keys 0"          ~: (Set.fromList [""]) ~=? (Anatree.keys 0 t),
  "member 'a'"      ~: True  ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: False ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: False ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: True  ~=? (Anatree.notMember "b" t),
  "keys 1"          ~: (Set.fromList ["a"]) ~=? (Anatree.keys 1 t),
  "member 'ab'"     ~: False ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList [])        ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ab'" ~: (Set.fromList ["", "a"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ba'"     ~: False ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList [])        ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ba'" ~: (Set.fromList ["", "a"]) ~=? (Anatree.subanagrams "ba" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t),
  "anagram 'ac'"    ~: (Set.fromList [])        ~=? (Anatree.anagrams "ac" t),
  "subanagram 'ac'" ~: (Set.fromList ["", "a"]) ~=? (Anatree.subanagrams "ac" t),
  "keys 2"          ~: (Set.fromList []) ~=? (Anatree.keys 2 t)
  ]

-- Tests with a single 'b' node [2]
tests_B :: Test.HUnit.Test
tests_B = let t = Anatree.fromList ["b"]
  in "['b']" ~: Test.HUnit.TestList [
  "size"            ~: 1     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 3     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "keys 0"          ~: (Set.fromList []) ~=? (Anatree.keys 0 t),
  "member 'a'"      ~: False ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: True  ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: True  ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: False ~=? (Anatree.notMember "b" t),
  "keys 1"          ~: (Set.fromList ["b"]) ~=? (Anatree.keys 1 t),
  "member 'ab'"     ~: False ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList [])    ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ab'" ~: (Set.fromList ["b"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ba'"     ~: False ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList [])    ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ba'" ~: (Set.fromList ["b"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t),
  "anagram 'ac'"    ~: (Set.fromList []) ~=? (Anatree.anagrams "ac" t),
  "subanagram 'ac'" ~: (Set.fromList []) ~=? (Anatree.subanagrams "ac" t),
  "keys 2"          ~: (Set.fromList []) ~=? (Anatree.keys 2 t)
  ]

-- Tests with an 'a,b' chain [2]
tests_A_B :: Test.HUnit.Test
tests_A_B = let t = Anatree.fromList ["a", "b"]
  in "['a','b']" ~: Test.HUnit.TestList [
  "size"            ~: 2     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 5     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "keys 0"          ~: (Set.fromList []) ~=? (Anatree.keys 0 t),
  "member 'a'"      ~: True  ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: False ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: True  ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: False ~=? (Anatree.notMember "b" t),
  "keys 1"          ~: (Set.fromList ["a", "b"]) ~=? (Anatree.keys 1 t),
  "member 'ab'"     ~: False ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList [])         ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ab'" ~: (Set.fromList ["a", "b"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ba'"     ~: False ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList [])         ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ba'" ~: (Set.fromList ["a", "b"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t),
  "anagram 'ac'"    ~: (Set.fromList [])    ~=? (Anatree.anagrams "ac" t),
  "subanagram 'ac'" ~: (Set.fromList ["a"]) ~=? (Anatree.subanagrams "ac" t),
  "keys 2"          ~: (Set.fromList []) ~=? (Anatree.keys 2 t)
  ]

tests_B_A :: Test.HUnit.Test
tests_B_A = let t = Anatree.fromList ["b", "a"]
  in "['b','a']" ~: Test.HUnit.TestList [
  "size"            ~: 2     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 5     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "keys 0"          ~: (Set.fromList []) ~=? (Anatree.keys 0 t),
  "member 'a'"      ~: True  ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: False ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: True  ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: False ~=? (Anatree.notMember "b" t),
  "keys 1"          ~: (Set.fromList ["a", "b"]) ~=? (Anatree.keys 1 t),
  "member 'ab'"     ~: False ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList [])         ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ab'" ~: (Set.fromList ["a", "b"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ba'"     ~: False ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList [])         ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ba'" ~: (Set.fromList ["a", "b"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t),
  "anagram 'ac'"    ~: (Set.fromList [])    ~=? (Anatree.anagrams "ac" t),
  "subanagram 'ac'" ~: (Set.fromList ["a"]) ~=? (Anatree.subanagrams "ac" t),
  "keys 2"          ~: (Set.fromList []) ~=? (Anatree.keys 2 t)
  ]

tests_AB_BA :: Test.HUnit.Test
tests_AB_BA = let t = Anatree.fromList ["ab", "ba"]
  in "['ab','ba']" ~: Test.HUnit.TestList [
  "size"            ~: 2     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 5     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "keys 0"          ~: (Set.fromList []) ~=? (Anatree.keys 0 t),
  "member 'a'"      ~: False ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: True  ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: False ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: True  ~=? (Anatree.notMember "b" t),
  "keys 1"          ~: (Set.fromList []) ~=? (Anatree.keys 1 t),
  "member 'ab'"     ~: True  ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList ["ab", "ba"]) ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ab'" ~: (Set.fromList ["ab", "ba"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ba'"     ~: True  ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList ["ab", "ba"]) ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ba'" ~: (Set.fromList ["ab", "ba"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t),
  "anagram 'ac'"    ~: (Set.fromList []) ~=? (Anatree.anagrams "ac" t),
  "subanagram 'ac'" ~: (Set.fromList []) ~=? (Anatree.subanagrams "ac" t),
  "keys 2"          ~: (Set.fromList ["ab"]) ~=? (Anatree.keys 2 t)
  ]

tests_BA_AB :: Test.HUnit.Test
tests_BA_AB = let t = Anatree.fromList ["ba", "ab"]
  in "['ba','ab']" ~: Test.HUnit.TestList [
  "size"            ~: 2     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 5     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "keys 0"          ~: (Set.fromList []) ~=? (Anatree.keys 0 t),
  "member 'a'"      ~: False ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: True  ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: False ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: True  ~=? (Anatree.notMember "b" t),
  "keys 1"          ~: (Set.fromList []) ~=? (Anatree.keys 1 t),
  "member 'ab'"     ~: True  ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList ["ab", "ba"]) ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ab'" ~: (Set.fromList ["ab", "ba"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ba'"     ~: True  ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList ["ab", "ba"]) ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ba'" ~: (Set.fromList ["ab", "ba"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t),
  "anagram 'ac'"    ~: (Set.fromList []) ~=? (Anatree.anagrams "ac" t),
  "subanagram 'ac'" ~: (Set.fromList []) ~=? (Anatree.subanagrams "ac" t),
  "keys 2"          ~: (Set.fromList ["ab"]) ~=? (Anatree.keys 2 t)
  ]

tests_A_AB_A :: Test.HUnit.Test
tests_A_AB_A = let t = Anatree.fromList ["a", "ab", "a"]
  in "['a','ab','a']" ~: Test.HUnit.TestList [
  "size"            ~: 2     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 5     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "keys 0"          ~: (Set.fromList []) ~=? (Anatree.keys 0 t),
  "member 'a'"      ~: True  ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: False ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: False ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: True  ~=? (Anatree.notMember "b" t),
  "keys 1"          ~: (Set.fromList ["a"]) ~=? (Anatree.keys 1 t),
  "member 'ab'"     ~: True  ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList ["ab"])      ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ab'" ~: (Set.fromList ["a", "ab"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ba'"     ~: False ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList ["ab"])      ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ba'" ~: (Set.fromList ["a", "ab"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t),
  "anagram 'ac'"    ~: (Set.fromList [])    ~=? (Anatree.anagrams "ac" t),
  "subanagram 'ac'" ~: (Set.fromList ["a"]) ~=? (Anatree.subanagrams "ac" t),
  "keys 2"          ~: (Set.fromList ["ab"]) ~=? (Anatree.keys 2 t)
  ]

-- Tests with one 'a' node and two 'b' nodes.
tests_B_AB :: Test.HUnit.Test
tests_B_AB = let t = Anatree.fromList ["b", "ab"]
  in "['b','ab']" ~: Test.HUnit.TestList [
  "size"            ~: 2     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 7     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "keys 0"          ~: (Set.fromList []) ~=? (Anatree.keys 0 t),
  "member 'a'"      ~: False ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: True  ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: True  ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: False ~=? (Anatree.notMember "b" t),
  "keys 1"          ~: (Set.fromList ["b"]) ~=? (Anatree.keys 1 t),
  "member 'ab'"     ~: True  ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList ["ab"])      ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ab'" ~: (Set.fromList ["b", "ab"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ba'"     ~: False ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList ["ab"])      ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ba'" ~: (Set.fromList ["b", "ab"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t),
  "anagram 'ac'"    ~: (Set.fromList []) ~=? (Anatree.anagrams "ac" t),
  "subanagram 'ac'" ~: (Set.fromList []) ~=? (Anatree.subanagrams "ac" t),
  "keys 2"          ~: (Set.fromList ["ab"]) ~=? (Anatree.keys 2 t)
  ]

tests_B_AB_B :: Test.HUnit.Test
tests_B_AB_B = let t = Anatree.fromList ["b", "ab", "b"]
  in "['b','ab','b']" ~: Test.HUnit.TestList [
  "size"            ~: 2     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 7     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "keys 0"          ~: (Set.fromList []) ~=? (Anatree.keys 0 t),
  "member 'a'"      ~: False ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: True  ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: True  ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: False ~=? (Anatree.notMember "b" t),
  "keys 1"          ~: (Set.fromList ["b"]) ~=? (Anatree.keys 1 t),
  "member 'ab'"     ~: True  ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList ["ab"])      ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ab'" ~: (Set.fromList ["b", "ab"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ba'"     ~: False ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList ["ab"])      ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ba'" ~: (Set.fromList ["b", "ab"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t),
  "anagram 'ac'"    ~: (Set.fromList []) ~=? (Anatree.anagrams "ac" t),
  "subanagram 'ac'" ~: (Set.fromList []) ~=? (Anatree.subanagrams "ac" t),
  "keys 2"          ~: (Set.fromList ["ab"]) ~=? (Anatree.keys 2 t)
  ]

tests_B_AB_BA :: Test.HUnit.Test
tests_B_AB_BA = let t = Anatree.fromList ["b", "ab", "ba"]
  in "['b','ab','ba']" ~: Test.HUnit.TestList [
  "size"            ~: 3     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 7     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "keys 0"          ~: (Set.fromList []) ~=? (Anatree.keys 0 t),
  "member 'a'"      ~: False ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: True  ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: True  ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: False ~=? (Anatree.notMember "b" t),
  "keys 1"          ~: (Set.fromList ["b"]) ~=? (Anatree.keys 1 t),
  "member 'ab'"     ~: True  ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList ["ab", "ba"])      ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ab'" ~: (Set.fromList ["b", "ab", "ba"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ba'"     ~: True  ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList ["ab", "ba"])      ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ba'" ~: (Set.fromList ["b", "ab", "ba"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t),
  "anagram 'ac'"    ~: (Set.fromList []) ~=? (Anatree.anagrams "ac" t),
  "subanagram 'ac'" ~: (Set.fromList []) ~=? (Anatree.subanagrams "ac" t),
  "keys 2"          ~: (Set.fromList ["ab"]) ~=? (Anatree.keys 2 t)
  ]

tests_B_AB_A_BA_Eta :: Test.HUnit.Test
tests_B_AB_A_BA_Eta = let t = Anatree.fromList ["b", "ab", "a", "ba", ""]
  in "['b','ab','a','ba','']" ~: Test.HUnit.TestList [
  "size"            ~: 5     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 7     ~=? (Anatree.treeSize t),
  "member ''"       ~: True  ~=? (Anatree.member "" t),
  "notMember ''"    ~: False ~=? (Anatree.notMember "" t),
  "keys 0"          ~: (Set.fromList [""]) ~=? (Anatree.keys 0 t),
  "member 'a'"      ~: True  ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: False ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: True  ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: False ~=? (Anatree.notMember "b" t),
  "keys 1"          ~: (Set.fromList ["a", "b"]) ~=? (Anatree.keys 1 t),
  "member 'ab'"     ~: True  ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList ["ab", "ba"])               ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ab'" ~: (Set.fromList ["", "a", "b", "ab", "ba"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ba'"     ~: True  ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList ["ab", "ba"])               ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ba'" ~: (Set.fromList ["", "a", "b", "ab", "ba"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t),
  "anagram 'ac'"    ~: (Set.fromList [])        ~=? (Anatree.anagrams "ac" t),
  "subanagram 'ac'" ~: (Set.fromList ["", "a"]) ~=? (Anatree.subanagrams "ac" t),
  "keys 2"          ~: (Set.fromList ["ab"]) ~=? (Anatree.keys 2 t)
  ]

-- An 'abc' chain.
tests_AC_BA :: Test.HUnit.Test
tests_AC_BA = let t = Anatree.fromList ["ac", "ba"]
  in "['ac','ba']" ~: Test.HUnit.TestList [
  "size"            ~: 2     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 7     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "keys 0"          ~: (Set.fromList []) ~=? (Anatree.keys 0 t),
  "member 'a'"      ~: False ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: True  ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: False ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: True  ~=? (Anatree.notMember "b" t),
  "keys 1"          ~: (Set.fromList []) ~=? (Anatree.keys 1 t),
  "member 'ab'"     ~: False ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList ["ba"]) ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ab'" ~: (Set.fromList ["ba"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ba'"     ~: True  ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList ["ba"]) ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ba'" ~: (Set.fromList ["ba"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ac'"     ~: True  ~=? (Anatree.member "ac" t),
  "anagram 'ac'"    ~: (Set.fromList ["ac"]) ~=? (Anatree.anagrams "ac" t),
  "subanagram 'ac'" ~: (Set.fromList ["ac"]) ~=? (Anatree.subanagrams "ac" t),
  "keys 2"          ~: (Set.fromList ["ba","ac"]) ~=? (Anatree.keys 2 t)
  ]

tests_AC_A_BA :: Test.HUnit.Test
tests_AC_A_BA = let t = Anatree.fromList ["ac", "a", "ba"]
  in "['ac','a','ba']" ~: Test.HUnit.TestList [
  "size"            ~: 3     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 7     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "keys 0"          ~: (Set.fromList []) ~=? (Anatree.keys 0 t),
  "member 'a'"      ~: True  ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: False ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: False ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: True  ~=? (Anatree.notMember "b" t),
  "keys 1"          ~: (Set.fromList ["a"]) ~=? (Anatree.keys 1 t),
  "member 'ab'"     ~: False ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList ["ba"]) ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ab'" ~: (Set.fromList ["a", "ba"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ba'"     ~: True  ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList ["ba"]) ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ba'" ~: (Set.fromList ["a", "ba"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ac'"     ~: True  ~=? (Anatree.member "ac" t),
  "anagram 'ac'"    ~: (Set.fromList ["ac"])      ~=? (Anatree.anagrams "ac" t),
  "subanagram 'ac'" ~: (Set.fromList ["a", "ac"]) ~=? (Anatree.subanagrams "ac" t),
  "keys 2"          ~: (Set.fromList ["ba","ac"]) ~=? (Anatree.keys 2 t)
  ]

-- Tree with a common 'a' node and then splitting into 'b' and 'c' subtrees
tests_AC_B_A :: Test.HUnit.Test
tests_AC_B_A = let t = Anatree.fromList ["ac", "b", "a"]
  in "['ac','b','a']" ~: Test.HUnit.TestList [
  "size"            ~: 3     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 7     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "keys 0"          ~: (Set.fromList []) ~=? (Anatree.keys 0 t),
  "member 'a'"      ~: True  ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: False ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: True  ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: False ~=? (Anatree.notMember "b" t),
  "keys 1"          ~: (Set.fromList ["a", "b"]) ~=? (Anatree.keys 1 t),
  "member 'ab'"     ~: False ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList [])         ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ab'" ~: (Set.fromList ["a", "b"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ba'"     ~: False ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList [])         ~=? (Anatree.anagrams "ab" t),
  "subanagram 'ba'" ~: (Set.fromList ["a", "b"]) ~=? (Anatree.subanagrams "ab" t),
  "member 'ac'"     ~: True  ~=? (Anatree.member "ac" t),
  "anagram 'ac'"    ~: (Set.fromList ["ac"])      ~=? (Anatree.anagrams "ac" t),
  "subanagram 'ac'" ~: (Set.fromList ["a", "ac"]) ~=? (Anatree.subanagrams "ac" t),
  "keys 2"          ~: (Set.fromList ["ac"]) ~=? (Anatree.keys 2 t)
  ]

-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests :: [Test.Framework.Test]
tests = hUnitTestToTests $ TestList [
  tests_Empty,
  tests_Eta,
  tests_A, tests_A_A, tests_Eta_A,
  tests_B,
  tests_A_B, tests_B_A, tests_BA_AB, tests_AB_BA, tests_A_AB_A,
  tests_B_AB, tests_B_AB_BA, tests_B_AB_B, tests_B_AB_A_BA_Eta,
  tests_AC_A_BA, tests_AC_BA,
  tests_AC_B_A
  ]

main :: IO ()
main = defaultMain tests
