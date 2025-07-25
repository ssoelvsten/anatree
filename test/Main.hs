module Main where
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Anatree as Anatree

test_emptyInfo :: Test.HUnit.Test
test_emptyInfo = let t = Anatree.empty ()
                 in Test.HUnit.TestList [
  "Empty tree has no words" ~: (Anatree.size t) ~=? 0,
  "Empty tree has size 1" ~: (Anatree.treeSize t) ~=? 1
  ]

-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests :: [Test.Framework.Test]
tests = hUnitTestToTests $ TestList [ test_emptyInfo ]

main :: IO ()
main = defaultMain tests
