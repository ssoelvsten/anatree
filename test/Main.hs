module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Data.Set as Set
import qualified Anatree as Anatree

-- Tests with the empty tree [2]
tests_Empty :: Test.HUnit.Test
tests_Empty = let t = Anatree.empty
  in "{}" ~: Test.HUnit.TestList [
  "size"            ~: 0     ~=? (Anatree.size t),
  "null"            ~: True  ~=? (Anatree.null t),
  "treeSize"        ~: 1     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "member 'a'"      ~: False ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: True  ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: False ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: True  ~=? (Anatree.notMember "b" t),
  "member 'ab'"     ~: False ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList []) ~=? (Anatree.anagram "ab" t),
  "member 'ba'"     ~: False ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList []) ~=? (Anatree.anagram "ab" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t)
  ]

tests_Eta :: Test.HUnit.Test
tests_Eta = let t = Anatree.insert "" $ Anatree.empty
  in "{''}" ~: Test.HUnit.TestList [
  "size"            ~: 1     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 1     ~=? (Anatree.treeSize t),
  "member ''"       ~: True  ~=? (Anatree.member "" t),
  "notMember ''"    ~: False ~=? (Anatree.notMember "" t),
  "member 'a'"      ~: False ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: True  ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: False ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: True  ~=? (Anatree.notMember "b" t),
  "member 'ab'"     ~: False ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList []) ~=? (Anatree.anagram "ab" t),
  "member 'ba'"     ~: False ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList []) ~=? (Anatree.anagram "ab" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t)
  ]

-- Tests with a single 'a' node [2]
tests_A :: Test.HUnit.Test
tests_A = let t = Anatree.insert "a" $ Anatree.empty
  in "{'a'}" ~: Test.HUnit.TestList [
  "size"            ~: 1     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 3     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "member 'a'"      ~: True  ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: False ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: False ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: True  ~=? (Anatree.notMember "b" t),
  "member 'ab'"     ~: False ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList []) ~=? (Anatree.anagram "ab" t),
  "member 'ba'"     ~: False ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList []) ~=? (Anatree.anagram "ab" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t)
  ]

tests_A_A :: Test.HUnit.Test
tests_A_A = let t = Anatree.insert "a" $ Anatree.insert "a" $ Anatree.empty
  in "{'a','a'}" ~: Test.HUnit.TestList [
  "size"            ~: 1     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 3     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "member 'a'"      ~: True  ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: False ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: False ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: True  ~=? (Anatree.notMember "b" t),
  "member 'ab'"     ~: False ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList []) ~=? (Anatree.anagram "ab" t),
  "member 'ba'"     ~: False ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList []) ~=? (Anatree.anagram "ab" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t)
  ]

tests_A_Eta :: Test.HUnit.Test
tests_A_Eta = let t = Anatree.insert "a" $ Anatree.insert "" $ Anatree.empty
  in "{'a',''}" ~: Test.HUnit.TestList [
  "size"            ~: 2     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 3     ~=? (Anatree.treeSize t),
  "member ''"       ~: True  ~=? (Anatree.member "" t),
  "notMember ''"    ~: False ~=? (Anatree.notMember "" t),
  "member 'a'"      ~: True  ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: False ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: False ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: True  ~=? (Anatree.notMember "b" t),
  "member 'ab'"     ~: False ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList []) ~=? (Anatree.anagram "ab" t),
  "member 'ba'"     ~: False ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList []) ~=? (Anatree.anagram "ab" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t)
  ]

-- Tests with a single 'b' node [2]
tests_B :: Test.HUnit.Test
tests_B = let t = Anatree.insert "b" Anatree.empty
  in "{'b'}" ~: Test.HUnit.TestList [
  "size"            ~: 1     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 3     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "member 'a'"      ~: False ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: True  ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: True  ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: False ~=? (Anatree.notMember "b" t),
  "member 'ab'"     ~: False ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList []) ~=? (Anatree.anagram "ab" t),
  "member 'ba'"     ~: False ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList []) ~=? (Anatree.anagram "ab" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t)
  ]

-- Tests with an 'a,b' chain [2]
tests_A_B :: Test.HUnit.Test
tests_A_B = let t = Anatree.insert "a" $ Anatree.insert "b" $ Anatree.empty
  in "{'a','b'}" ~: Test.HUnit.TestList [
  "size"            ~: 2     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 5     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "member 'a'"      ~: True  ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: False ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: True  ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: False ~=? (Anatree.notMember "b" t),
  "member 'ab'"     ~: False ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList []) ~=? (Anatree.anagram "ab" t),
  "member 'ba'"     ~: False ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList []) ~=? (Anatree.anagram "ab" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t)
  ]

tests_B_A :: Test.HUnit.Test
tests_B_A = let t = Anatree.insert "b" $ Anatree.insert "a" $ Anatree.empty
  in "{'b','a'}" ~: Test.HUnit.TestList [
  "size"            ~: 2     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 5     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "member 'a'"      ~: True  ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: False ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: True  ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: False ~=? (Anatree.notMember "b" t),
  "member 'ab'"     ~: False ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList []) ~=? (Anatree.anagram "ab" t),
  "member 'ba'"     ~: False ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList []) ~=? (Anatree.anagram "ab" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t)
  ]

tests_AB_BA :: Test.HUnit.Test
tests_AB_BA = let t = Anatree.insert "ab" $ Anatree.insert "ba" $ Anatree.empty
  in "{'ab','ba'}" ~: Test.HUnit.TestList [
  "size"            ~: 2     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 5     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "member 'a'"      ~: False ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: True  ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: False ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: True  ~=? (Anatree.notMember "b" t),
  "member 'ab'"     ~: True  ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList ["ab", "ba"]) ~=? (Anatree.anagram "ab" t),
  "member 'ba'"     ~: True  ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList ["ab", "ba"]) ~=? (Anatree.anagram "ab" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t)
  ]

tests_BA_AB :: Test.HUnit.Test
tests_BA_AB = let t = Anatree.insert "ba" $ Anatree.insert "ab" $ Anatree.empty
  in "{'ba','ab'}" ~: Test.HUnit.TestList [
  "size"            ~: 2     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 5     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "member 'a'"      ~: False ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: True  ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: False ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: True  ~=? (Anatree.notMember "b" t),
  "member 'ab'"     ~: True  ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList ["ab", "ba"]) ~=? (Anatree.anagram "ab" t),
  "member 'ba'"     ~: True  ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList ["ab", "ba"]) ~=? (Anatree.anagram "ab" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t)
  ]

tests_A_AB_A :: Test.HUnit.Test
tests_A_AB_A = let t = Anatree.insert "a" $ Anatree.insert "ab"
                     $ Anatree.insert "a" $ Anatree.empty
  in "{'a','ab','a'}" ~: Test.HUnit.TestList [
  "size"            ~: 2     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 5     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "member 'a'"      ~: True  ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: False ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: False ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: True  ~=? (Anatree.notMember "b" t),
  "member 'ab'"     ~: True  ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList ["ab"]) ~=? (Anatree.anagram "ab" t),
  "member 'ba'"     ~: False ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList ["ab"]) ~=? (Anatree.anagram "ab" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t)
  ]

-- Tests with one 'a' node and two 'b' nodes.
tests_AB_B :: Test.HUnit.Test
tests_AB_B = let t = Anatree.insert "ab" $ Anatree.insert "b" $ Anatree.empty
  in "{'ab','b'}" ~: Test.HUnit.TestList [
  "size"            ~: 2     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 7     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "member 'a'"      ~: False ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: True  ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: True  ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: False ~=? (Anatree.notMember "b" t),
  "member 'ab'"     ~: True  ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList ["ab"]) ~=? (Anatree.anagram "ab" t),
  "member 'ba'"     ~: False ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList ["ab"]) ~=? (Anatree.anagram "ab" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t)
  ]

tests_B_AB_B :: Test.HUnit.Test
tests_B_AB_B = let t = Anatree.insert "b" $ Anatree.insert "ab"
                     $ Anatree.insert "b" $ Anatree.empty
  in "{'b','ab','b'}" ~: Test.HUnit.TestList [
  "size"            ~: 2     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 7     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "member 'a'"      ~: False ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: True  ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: True  ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: False ~=? (Anatree.notMember "b" t),
  "member 'ab'"     ~: True  ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList ["ab"]) ~=? (Anatree.anagram "ab" t),
  "member 'ba'"     ~: False ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList ["ab"]) ~=? (Anatree.anagram "ab" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t)
  ]

tests_BA_AB_B :: Test.HUnit.Test
tests_BA_AB_B = let t = Anatree.insert "ba" $ Anatree.insert "ab"
                      $ Anatree.insert "b"  $ Anatree.empty
  in "{'ba','ab','b'}" ~: Test.HUnit.TestList [
  "size"            ~: 3     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 7     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "member 'a'"      ~: False ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: True  ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: True  ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: False ~=? (Anatree.notMember "b" t),
  "member 'ab'"     ~: True  ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList ["ab", "ba"]) ~=? (Anatree.anagram "ab" t),
  "member 'ba'"     ~: True  ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList ["ab", "ba"]) ~=? (Anatree.anagram "ab" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t)
  ]

tests_Eta_BA_A_AB_B :: Test.HUnit.Test
tests_Eta_BA_A_AB_B = let t = Anatree.insert ""  $ Anatree.insert "ba"
                            $ Anatree.insert "a" $ Anatree.insert "ab"
                            $ Anatree.insert "b" $ Anatree.empty
  in "{'','ba','a','ab','b'}" ~: Test.HUnit.TestList [
  "size"            ~: 5     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 7     ~=? (Anatree.treeSize t),
  "member ''"       ~: True  ~=? (Anatree.member "" t),
  "notMember ''"    ~: False ~=? (Anatree.notMember "" t),
  "member 'a'"      ~: True  ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: False ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: True  ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: False ~=? (Anatree.notMember "b" t),
  "member 'ab'"     ~: True  ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList ["ab", "ba"]) ~=? (Anatree.anagram "ab" t),
  "member 'ba'"     ~: True  ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList ["ab", "ba"]) ~=? (Anatree.anagram "ab" t),
  "member 'ac'"     ~: False ~=? (Anatree.member "ac" t)
  ]

-- An 'abc' chain.
tests_BA_A_AC :: Test.HUnit.Test
tests_BA_A_AC = let t = Anatree.insert "ba" $ Anatree.insert "a"
                      $ Anatree.insert "ac" $ Anatree.empty
  in "{'ba', 'a', 'ac'}" ~: Test.HUnit.TestList [
  "size"            ~: 3     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 7     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "member 'a'"      ~: True  ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: False ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: False ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: True  ~=? (Anatree.notMember "b" t),
  "member 'ab'"     ~: False ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList ["ba"]) ~=? (Anatree.anagram "ab" t),
  "member 'ba'"     ~: True  ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList ["ba"]) ~=? (Anatree.anagram "ab" t),
  "member 'ac'"     ~: True  ~=? (Anatree.member "ac" t)
  ]

-- Tree with a common 'a' node and then splitting into 'b' and 'c' subtrees
tests_BA_AC :: Test.HUnit.Test
tests_BA_AC = let t = Anatree.insert "ba"  $ Anatree.insert "ac" $ Anatree.empty
  in "{'ba','ac'}" ~: Test.HUnit.TestList [
  "size"            ~: 2     ~=? (Anatree.size t),
  "null"            ~: False ~=? (Anatree.null t),
  "treeSize"        ~: 7     ~=? (Anatree.treeSize t),
  "member ''"       ~: False ~=? (Anatree.member "" t),
  "notMember ''"    ~: True  ~=? (Anatree.notMember "" t),
  "member 'a'"      ~: False ~=? (Anatree.member "a" t),
  "notMember 'a'"   ~: True  ~=? (Anatree.notMember "a" t),
  "member 'b'"      ~: False ~=? (Anatree.member "b" t),
  "notMember 'b'"   ~: True  ~=? (Anatree.notMember "b" t),
  "member 'ab'"     ~: False ~=? (Anatree.member "ab" t),
  "anagram 'ab'"    ~: (Set.fromList ["ba"]) ~=? (Anatree.anagram "ab" t),
  "member 'ba'"     ~: True  ~=? (Anatree.member "ba" t),
  "anagram 'ba'"    ~: (Set.fromList ["ba"]) ~=? (Anatree.anagram "ab" t),
  "member 'ac'"     ~: True  ~=? (Anatree.member "ac" t)
  ]

-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests :: [Test.Framework.Test]
tests = hUnitTestToTests $ TestList [
  tests_Empty,
  tests_Eta,
  tests_A, tests_A_A, tests_A_Eta,
  tests_B,
  tests_A_B, tests_B_A, tests_BA_AB, tests_AB_BA, tests_A_AB_A,
  tests_AB_B, tests_BA_AB_B, tests_B_AB_B, tests_Eta_BA_A_AB_B,
  tests_BA_A_AC,
  tests_BA_AC
  ]

main :: IO ()
main = defaultMain tests
