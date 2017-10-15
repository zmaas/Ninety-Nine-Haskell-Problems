-- file Spec.hs
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.Hspec
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import OneThroughTen as OTT
import ElevenThroughTwenty as ETT

main :: IO ()
main = do
  unitTestsOneTen <- testSpec "(Tested by HSpec)" oneThroughTenSpec
  unitTestsElevenTwenty <- testSpec "(Tested by HSpec)" elevenThroughTwentySpec
  defaultMain (testGroup "tests" [unitTestsOneTen,
                                  unitTestsElevenTwenty])

oneThroughTenSpec :: Spec
oneThroughTenSpec = parallel $ do
  describe "Problems 1-10" $ do
    it "1. myLast returns the last element of a list" $ do
      OTT.myLast [1,2,3] `shouldBe` (3 :: Int)

    it "2. myButLast returns the 2nd to last element of a list" $ do
      OTT.myButLast [1,2,3] `shouldBe` (2 :: Int)

    it "3. elementAt returns the nth to last element of a list" $ do
      OTT.elementAt [1,2,3,4,5] 4 `shouldBe` (4 :: Int)

    it "4. myLength returns the length of a list" $ do
      OTT.myLength [1,2,3,4,5] `shouldBe` (5 :: Int)

    it "5. myReverse reverses a list" $ do
      OTT.myReverse [1,2,3,4,5] `shouldBe` ([5,4,3,2,1] :: [Int])

    it "6. isPalindrome reverses a list" $ do
      OTT.isPalindrome [1,2,3,2,1] `shouldBe` (True :: Bool)

    it "7. myFlatten flattens a nested list" $ do
      OTT.myFlatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
            `shouldBe` ([1,2,3,4,5] :: [Int])

    it "8. compress removes duplicates" $ do
      OTT.compress [1,2,2,3,3] `shouldBe` ([1,2,3] :: [Int])

    it "9. pack groups by repetition" $ do
      OTT.pack [1,2,2,3,3,3] `shouldBe` ([[1],[2,2],[3,3,3]] :: [[Int]])

    it "10. count subgroups by repetition" $ do
      OTT.encode [1,2,2,3,3,3] `shouldBe` ([(1,1),(2,2),(3,3)] :: [(Int, Int)])

elevenThroughTwentySpec :: Spec
elevenThroughTwentySpec = parallel $ do
  describe "Problems 11-20" $ do
    it "11. count subgroups by repetition, keeping singletons" $ do
      ETT.encodeModified [1,2,2,3,3,3]
        `shouldBe` ([Single 1, Multiple 2 2, Multiple 3 3] :: [ListItem Int])

    it "12. decode an encoded list" $ do
      ETT.decodeModified [Single 1, Multiple 2 2, Multiple 3 3]
        `shouldBe` ([1,2,2,3,3,3] :: [Int])

    it "13. count subgroups by repetition directly" $ do
      ETT.encodeModified [1,2,2,3,3,3]
        `shouldBe` ([Single 1, Multiple 2 2, Multiple 3 3] :: [ListItem Int])

    it "14. duplicate elements in a list" $ do
      ETT.dupli [1,2,3]
        `shouldBe` ([1,1,2,2,3,3] :: [Int])

    it "15. replicate elements in a list n times" $ do
      (ETT.repli [1,2,3] 3)
        `shouldBe` ([1,1,1,2,2,2,3,3,3] :: [Int])

    it "16. drop nth elements from a list" $ do
      (ETT.myDrop [1,2,3,4,5,6] 3)
        `shouldBe` ([1,2,4,5] :: [Int])

    it "17. split a list at the nth element" $ do
      (ETT.mySplit [1,2,3,4,5,6] 3)
        `shouldBe` (([1,2,3],[4,5,6]) :: ([Int],[Int]))

    it "18. slice a list between the nth and mth element" $ do
      (ETT.mySlice [1,2,3,4,5,6,7,8,9,10] 3 7)
        `shouldBe` ([3,4,5,6,7] :: [Int])

    it "19p. rotate a list (positive)" $ do
      (ETT.rotate [1,2,3,4,5,6,7] 3)
        `shouldBe` ([4,5,6,7,1,2,3] :: [Int])

    it "19n. rotate a list (negative)" $ do
      (ETT.rotate [1,2,3,4,5,6,7] (-2))
        `shouldBe` ([6,7,1,2,3,4,5] :: [Int])

    it "20. remove the nth element of a list)" $ do
      (ETT.removeAt [1,2,3,4,5,6,7] 3)
        `shouldBe` ([1,2,4,5,6,7] :: [Int])
