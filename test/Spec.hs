-- file Spec.hs
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import OneThroughTen

main :: IO ()
main = hspec $ do
  describe "Problems 1-10" $ do
    it "myLast returns the last element of a list" $ do
      OneThroughTen.myLast [1,2,3] `shouldBe` (3 :: Int)

    it "myButLast returns the 2nd to last element of a list" $ do
      OneThroughTen.myButLast [1,2,3] `shouldBe` (2 :: Int)

    it "elementAt returns the nth to last element of a list" $ do
      OneThroughTen.elementAt [1,2,3,4,5] 4 `shouldBe` (4 :: Int)

    it "myLength returns the length of a list" $ do
      OneThroughTen.myLength [1,2,3,4,5] `shouldBe` (5 :: Int)

    it "myReverse reverses a list" $ do
      OneThroughTen.myReverse [1,2,3,4,5] `shouldBe` ([5,4,3,2,1] :: [Int])

    it "isPalindrome reverses a list" $ do
      OneThroughTen.isPalindrome [1,2,3,2,1] `shouldBe` (True :: Bool)

    it "myFlatten flattens a nested list" $ do
      OneThroughTen.myFlatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
        `shouldBe` ([1,2,3,4,5] :: [Int])

    it "compress removes duplicates" $ do
      OneThroughTen.compress [1,2,2,3,3] `shouldBe` ([1,2,3] :: [Int])

    it "pack groups by repetition" $ do
      OneThroughTen.pack [1,2,2,3,3,3] `shouldBe` ([[1],[2,2],[3,3,3]] :: [[Int]])

    it "count subgroups by repetition" $ do
      OneThroughTen.encode [1,2,2,3,3,3] `shouldBe` ([(1,1),(2,2),(3,3)] :: [(Int, Int)])
