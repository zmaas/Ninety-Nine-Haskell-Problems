{-# LANGUAGE OverloadedStrings #-}
module OneThroughTen
  ( someFunc,
      myLast,
      myButLast,
      elementAt,
      myLength,
      myReverse,
      isPalindrome,
      NestedList (Elem, List),
      myFlatten,
      compress,
      pack,
      encode
    ) where

import qualified Data.Text.IO as T
import Data.List

someFunc :: IO ()
someFunc = T.putStrLn "Hello Hask World!"

-- Problem 1
myLast :: [a] -> a
myLast [] = error "Can't get the last element of an empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast = last . init

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt (x:_) 1  = x
elementAt [x] _ = error "Out of bounds"
elementAt (_:xs) i = elementAt xs (i-1)

-- Problem 4
myLength :: (Num b) => [a] -> b
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)
isPalindrome' []  = True
isPalindrome' [_] = True
isPalindrome' xs  = (head xs) == (last xs) && (isPalindrome' $ init $ tail xs)

-- Problem 7
data NestedList a = Elem a | List [NestedList a]
myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List x) = concatMap myFlatten x

-- Problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : (compress $ dropWhile (== x) xs)

 -- Problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:filter (== x) xs):(pack $ filter (/= x) xs)

 -- Problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) (group xs)
