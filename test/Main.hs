module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Anatree as Anatree

-- Tests with the empty tree [2]
tests_Empty :: Test.HUnit.Test
tests_Empty = let t = Anatree.empty
  in "{}" ~: Test.HUnit.TestList [
  "Has size 0"            ~: 0     ~=? (Anatree.size t),
  "Has a tree of size 1"  ~: 1     ~=? (Anatree.treeSize t),
  "Does not contain ''"   ~: False ~=? (Anatree.member "" t),
  "Does not contain 'a'"  ~: False ~=? (Anatree.member "a" t),
  "Does not contain 'b'"  ~: False ~=? (Anatree.member "b" t),
  "Does not contain 'ab'" ~: False ~=? (Anatree.member "ab" t),
  "Does not contain 'ba'" ~: False ~=? (Anatree.member "ba" t)
  ]

tests_Eta :: Test.HUnit.Test
tests_Eta = let t = Anatree.insert "" $ Anatree.empty
  in "{''}" ~: Test.HUnit.TestList [
  "Has size 1"            ~: 1     ~=? (Anatree.size t),
  "Has a tree of size 1"  ~: 1     ~=? (Anatree.treeSize t),
  "Contains ''"           ~: True  ~=? (Anatree.member "" t),
  "Does not contain 'a'"  ~: False ~=? (Anatree.member "a" t),
  "Does not contain 'b'"  ~: False ~=? (Anatree.member "b" t),
  "Does not contain 'ab'" ~: False ~=? (Anatree.member "ab" t),
  "Does not contain 'ba'" ~: False ~=? (Anatree.member "ba" t)
  ]

-- Tests with a single 'a' node [2]
tests_A :: Test.HUnit.Test
tests_A = let t = Anatree.insert "a" $ Anatree.empty
  in "{'a'}" ~: Test.HUnit.TestList [
  "Has size 1"            ~: 1     ~=? (Anatree.size t),
  "Has a tree of size 3"  ~: 3     ~=? (Anatree.treeSize t),
  "Does not contains ''"  ~: False ~=? (Anatree.member "" t),
  "Contains 'a'"          ~: True  ~=? (Anatree.member "a" t),
  "Does not contain 'b'"  ~: False ~=? (Anatree.member "b" t),
  "Does not contain 'ab'" ~: False ~=? (Anatree.member "ab" t),
  "Does not contain 'ba'" ~: False ~=? (Anatree.member "ba" t)
  ]

tests_A_A :: Test.HUnit.Test
tests_A_A = let t = Anatree.insert "a" $ Anatree.insert "a" $ Anatree.empty
  in "{'a'}" ~: Test.HUnit.TestList [
  "Has size 1"            ~: 1     ~=? (Anatree.size t),
  "Has a tree of size 3"  ~: 3     ~=? (Anatree.treeSize t),
  "Does not contains ''"  ~: False ~=? (Anatree.member "" t),
  "Contains 'a'"          ~: True  ~=? (Anatree.member "a" t),
  "Does not contain 'b'"  ~: False ~=? (Anatree.member "b" t),
  "Does not contain 'ab'" ~: False ~=? (Anatree.member "ab" t),
  "Does not contain 'ba'" ~: False ~=? (Anatree.member "ba" t)
  ]

-- Tests with a single 'b' node [2]
tests_B :: Test.HUnit.Test
tests_B = let t = Anatree.insert "b" Anatree.empty
  in "{'b'}" ~: Test.HUnit.TestList [
  "Has size 1"            ~: 1     ~=? (Anatree.size t),
  "Has a tree of size 3"  ~: 3     ~=? (Anatree.treeSize t),
  "Does not contains ''"  ~: False ~=? (Anatree.member "" t),
  "Does not contain 'a'"  ~: False ~=? (Anatree.member "a" t),
  "Contains 'b'"          ~: True  ~=? (Anatree.member "b" t),
  "Does not contain 'ab'" ~: False ~=? (Anatree.member "ab" t),
  "Does not contain 'ba'" ~: False ~=? (Anatree.member "ba" t)
  ]

-- Tests with an 'a,b' chain [2]
tests_A_B :: Test.HUnit.Test
tests_A_B = let t = Anatree.insert "a" $ Anatree.insert "b" $ Anatree.empty
  in "{'a','b'}" ~: Test.HUnit.TestList [
  "Has size 2"            ~: 2     ~=? (Anatree.size t),
  "Has a tree of size 5"  ~: 5     ~=? (Anatree.treeSize t),
  "Does not contains ''"  ~: False ~=? (Anatree.member "" t),
  "Contains 'a'"          ~: True  ~=? (Anatree.member "a" t),
  "Contains 'b'"          ~: True  ~=? (Anatree.member "b" t),
  "Does not contain 'ab'" ~: False ~=? (Anatree.member "ab" t),
  "Does not contain 'ba'" ~: False ~=? (Anatree.member "ba" t)
  ]

tests_B_A :: Test.HUnit.Test
tests_B_A = let t = Anatree.insert "b" $ Anatree.insert "a" $ Anatree.empty
  in "{'a','b'}" ~: Test.HUnit.TestList [
  "Has size 2"            ~: 2     ~=? (Anatree.size t),
  "Has a tree of size 5"  ~: 5     ~=? (Anatree.treeSize t),
  "Does not contains ''"  ~: False ~=? (Anatree.member "" t),
  "Contains 'a'"          ~: True  ~=? (Anatree.member "a" t),
  "Contains 'b'"          ~: True  ~=? (Anatree.member "b" t),
  "Does not contain 'ab'" ~: False ~=? (Anatree.member "ab" t),
  "Does not contain 'ba'" ~: False ~=? (Anatree.member "ba" t)
  ]

tests_AB_BA :: Test.HUnit.Test
tests_AB_BA = let t = Anatree.insert "ab" $ Anatree.insert "ba" $ Anatree.empty
  in "{'ab','ba'}" ~: Test.HUnit.TestList [
  "Has size 2"            ~: 2     ~=? (Anatree.size t),
  "Has a tree of size 5"  ~: 5     ~=? (Anatree.treeSize t),
  "Does not contains ''"  ~: False ~=? (Anatree.member "" t),
  "Does not contain 'a'"  ~: False ~=? (Anatree.member "a" t),
  "Does not contain 'b'"  ~: False ~=? (Anatree.member "b" t),
  "Contains 'ab'"         ~: True  ~=? (Anatree.member "ab" t),
  "Contains 'ba'"         ~: True  ~=? (Anatree.member "ba" t)
  ]

tests_BA_AB :: Test.HUnit.Test
tests_BA_AB = let t = Anatree.insert "ba" $ Anatree.insert "ab" $ Anatree.empty
  in "{'ba','ab'}" ~: Test.HUnit.TestList [
  "Has size 2"            ~: 2     ~=? (Anatree.size t),
  "Has a tree of size 5"  ~: 5     ~=? (Anatree.treeSize t),
  "Does not contains ''"  ~: False ~=? (Anatree.member "" t),
  "Does not contain 'a'"  ~: False ~=? (Anatree.member "a" t),
  "Does not contain 'b'"  ~: False ~=? (Anatree.member "b" t),
  "Contains 'ab'"         ~: True  ~=? (Anatree.member "ab" t),
  "Contains 'ba'"         ~: True  ~=? (Anatree.member "ba" t)
  ]

tests_A_AB_A :: Test.HUnit.Test
tests_A_AB_A = let t = Anatree.insert "a" $ Anatree.insert "ab"
                     $ Anatree.insert "a" $ Anatree.empty
  in "{'a','ab', 'a'}" ~: Test.HUnit.TestList [
  "Has size 2"            ~: 2     ~=? (Anatree.size t),
  "Has a tree of size 5"  ~: 5     ~=? (Anatree.treeSize t),
  "Does not contains ''"  ~: False ~=? (Anatree.member "" t),
  "Contains 'a'"          ~: True  ~=? (Anatree.member "a" t),
  "Does not contain 'b'"  ~: False ~=? (Anatree.member "b" t),
  "Contains 'ab'"         ~: True  ~=? (Anatree.member "ab" t),
  "Does not contain 'ba'" ~: False ~=? (Anatree.member "ba" t)
  ]

-- Tests with one 'a' node and two 'b' nodes.
tests_AB_B :: Test.HUnit.Test
tests_AB_B = let t = Anatree.insert "ab" $ Anatree.insert "b" $ Anatree.empty
  in "{'ab','b'}" ~: Test.HUnit.TestList [
  "Has size 2"            ~: 2     ~=? (Anatree.size t),
  "Has a tree of size 5"  ~: 7     ~=? (Anatree.treeSize t),
  "Does not contains ''"  ~: False ~=? (Anatree.member "" t),
  "Does not contain 'a'"  ~: False ~=? (Anatree.member "a" t),
  "Contains 'b'"          ~: True  ~=? (Anatree.member "b" t),
  "Contains 'ab'"         ~: True  ~=? (Anatree.member "ab" t),
  "Does not contain 'ba'" ~: False ~=? (Anatree.member "ba" t)
  ]

tests_B_AB_B :: Test.HUnit.Test
tests_B_AB_B = let t = Anatree.insert "b" $ Anatree.insert "ab"
                     $ Anatree.insert "b" $ Anatree.empty
  in "{'b','ab', 'b'}" ~: Test.HUnit.TestList [
  "Has size 2"            ~: 2     ~=? (Anatree.size t),
  "Has a tree of size 5"  ~: 7     ~=? (Anatree.treeSize t),
  "Does not contains ''"  ~: False ~=? (Anatree.member "" t),
  "Does not contain 'a'"  ~: False ~=? (Anatree.member "a" t),
  "Contains 'b'"          ~: True  ~=? (Anatree.member "b" t),
  "Contains 'ab'"         ~: True  ~=? (Anatree.member "ab" t),
  "Does not contain 'ba'" ~: False ~=? (Anatree.member "ba" t)
  ]

tests_BA_AB_B :: Test.HUnit.Test
tests_BA_AB_B = let t = Anatree.insert "ba" $ Anatree.insert "ab"
                      $ Anatree.insert "b"  $ Anatree.empty
  in "{'ba','ab', 'b'}" ~: Test.HUnit.TestList [
  "Has size 3"            ~: 3     ~=? (Anatree.size t),
  "Has a tree of size 7"  ~: 7     ~=? (Anatree.treeSize t),
  "Does not contains ''"  ~: False ~=? (Anatree.member "" t),
  "Does not contain 'a'"  ~: False ~=? (Anatree.member "a" t),
  "Contains 'b'"          ~: True  ~=? (Anatree.member "b" t),
  "Contains 'ab'"         ~: True  ~=? (Anatree.member "ab" t),
  "Contains 'ba'"         ~: True  ~=? (Anatree.member "ba" t)
  ]

tests_Eta_BA_A_AB_B :: Test.HUnit.Test
tests_Eta_BA_A_AB_B = let t = Anatree.insert ""  $ Anatree.insert "ba"
                            $ Anatree.insert "a" $ Anatree.insert "ab"
                            $ Anatree.insert "b" $ Anatree.empty
  in "{'', 'ba', 'a', 'ab', 'b'}" ~: Test.HUnit.TestList [
  "Has size 5"           ~: 5     ~=? (Anatree.size t),
  "Has a tree of size 7" ~: 7     ~=? (Anatree.treeSize t),
  "Contains ''"          ~: True  ~=? (Anatree.member "" t),
  "Contain  'a'"         ~: True  ~=? (Anatree.member "a" t),
  "Contains 'b'"         ~: True  ~=? (Anatree.member "b" t),
  "Contains 'ab'"        ~: True  ~=? (Anatree.member "ab" t),
  "Contains 'ba'"        ~: True  ~=? (Anatree.member "ba" t)
  ]

-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests :: [Test.Framework.Test]
tests = hUnitTestToTests $ TestList [
  tests_Empty,
  tests_Eta,
  tests_A, tests_A_A,
  tests_B,
  tests_A_B, tests_B_A, tests_BA_AB, tests_AB_BA, tests_A_AB_A,
  tests_AB_B, tests_BA_AB_B, tests_B_AB_B, tests_Eta_BA_A_AB_B
  ]

main :: IO ()
main = defaultMain tests
