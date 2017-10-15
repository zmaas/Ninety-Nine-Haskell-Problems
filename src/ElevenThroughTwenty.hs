{-# LANGUAGE OverloadedStrings #-}
module ElevenThroughTwenty
  ( encodeModified,
    ListItem (Single, Multiple),
    decodeModified,
    encodeDirect,
    dupli,
    repli,
    myDrop,
    mySplit,
    mySlice,
    rotate,
    removeAt
  ) where

import Data.List
import OneThroughTen as OTT

-- Problem 11
data ListItem a = Single a | Multiple Int a
  deriving (Show, Eq)
encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map encodeModifier . OTT.encode
  where
    encodeModifier (1,x) = Single x
    encodeModifier (n,x) = Multiple n x

-- Problem 12
decodeModified :: Eq a => [ListItem a] -> [a]
decodeModified [] = []
decodeModified (Single x:xs) = [x] ++ (decodeModified xs)
decodeModified (Multiple n x:xs) = (replicate n x) ++ (decodeModified xs)

-- Problem 13
encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect (x:xs)
  | count == 1 = (Single x) : (encodeDirect xs)
  | otherwise = (Multiple count x ) : (encodeDirect rest)
  where
    (matched, rest) = span (==x) xs
    count = 1 + (length matched)

-- Problem 14
dupli :: Eq a => [a] -> [a]
dupli [] = []
dupli (x:xs) = [x,x] ++ (dupli xs)

-- Problem 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli xs n = concatMap (replicate n) xs

-- Problem 16
myDrop :: [a] -> Int -> [a]
myDrop [] _ = []
myDrop xs n
  | length xs < n = xs
  | otherwise = take (n-1) xs ++ myDrop (drop n xs) n

-- Problem 17
mySplit :: [a] -> Int -> ([a],[a])
mySplit [] _ = ([],[])
mySplit (x:xs) n
  | n > 0 = let (f,l) = mySplit xs (n-1) in (x : f, l)
mySplit xs _ = ([], xs)

-- Problem 18
mySlice :: [a] -> Int -> Int -> [a]
mySlice [] _ _ = []
mySlice xs n m = take (m-(n-1)) $ snd (mySplit xs (n-1))

-- Problem 19
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs n
  | n == 0 = xs
  | n > 0 = (drop n xs) ++ (take n xs)
  | n < 0 = (drop nfix xs) ++ (take nfix xs)
    where nfix = length xs + n

removeAt :: [a] -> Int -> [a]
removeAt [] _ = []
removeAt xs n = (init (fst xsplit)) ++ (snd xsplit)
  where xsplit = mySplit xs n
