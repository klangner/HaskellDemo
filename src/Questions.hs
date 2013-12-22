{-# LANGUAGE TemplateHaskell #-}
module Sandbox where

import System.Random
import Test.QuickCheck
import Test.QuickCheck.All


data QuickList a = ListAndIndex [a] Int deriving (Show)

instance (Arbitrary a) => Arbitrary (QuickList a) where
  arbitrary = do
    as <- listOf1 arbitrary           -- length xs > 0
    b <- choose (1,length as)         -- b > 0 and b <= length xs
    return (ListAndIndex as $ fromIntegral b)


-- Q1 Last element
last2 :: [a] -> a
last2 [x] = x
last2 (_:xs) = (last2 xs)
-- QuickCheck
prop_last2 xs = not (null xs) ==> last2 xs == last xs


-- Q2 Last but one
butLast :: [a] -> a
butLast (x:[_]) = x
butLast (_:xs) = (butLast xs)
-- QuickCheck
prop_butLast xs = length xs > 1 ==> butLast xs == xs!!((length xs)-2)


-- Q3 Element at
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt [] _ = error "Index out of bounds"
elementAt (_:xs) n = elementAt xs (n-1)
-- QuickCheck
prop_elementAt (ListAndIndex xs n) = elementAt xs n == xs!!(n - 1)


-- Q4 List length
length2 :: [a] -> Int
length2 [] = 0
length2 (_:xs) = (length2 xs) + 1
-- QuickCheck
prop_length xs = length2 xs == length xs


-- Q5 reverse list
reverse2 :: [a] -> [a]
reverse2 [] = []
reverse2 xs = reverse3 xs []
    where reverse3 [x] ys = (x:ys)
          reverse3 (x:xs) ys = reverse3 xs (x:ys)
-- QuickCheck
prop_reverse xs = reverse2 xs == reverse xs


-- Q6 Find out whether a list is a palindrome. 
-- A palindrome can be read forward or backward; e.g. (x a m a x). 
cmp :: Eq a => [a] -> [a] -> Bool
cmp [] _ = False
cmp _ [] = False
cmp [x] [y] = x == y
cmp (x:xs) (y:ys) | (x == y) = cmp xs ys
                  | otherwise = False

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == (reverse xs)
-- QuickCheck
prop_palindromeTrue = isPalindrome "matam"
prop_palindromeFalse = not $ isPalindrome "matar"


-- Q7 Flatten list
flatten :: [[a]] -> [a]
flatten [] = []
flatten [xs] = xs
flatten (x:xs) = x ++ flatten xs
-- QuickCheck
prop_flatten xs = length (flatten xs) == sum (map length xs)


-- Q8 Eliminate consecutive duplicates of list elements.
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs) | x == y = compress (y:xs)
                  | otherwise = (x:compress (y:xs))
-- QuickCheck
prop_compress = compress "aaleerd" == "alerd"


-- Q9 Pack consecutive duplicates of list elements into sublists. 
-- If a list contains repeated elements they should be placed in separate sublists.
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = reverse $ pack2 xs [x] []
    where pack2 [] ys zs = (ys:zs)
          pack2 (x:xs) (y:ys) zs | x == y = pack2 xs (x:y:ys) zs
                                 | otherwise = pack2 xs [x] ((y:ys):zs)
-- QuickCheck
prop_pack = pack "aasddeee" == ["aa", "s", "dd", "eee"]


-- Q10  Run-length encoding of a list. 
-- Use the result of problem P09 to implement the so-called 
-- run-length encoding data compression method. 
-- Consecutive duplicates of elements are encoded as lists (N E) 
-- where N is the number of duplicates of the element E.
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode xs = [(length (y:ys), y) | (y:ys) <- pack xs]
-- QuickCheck
prop_encode = encode "aaaabccaadeeee" == [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]


-- Q11 Modified run-length encoding.
-- Modify the result of problem 10 in such a way 
-- that if an element has no duplicates it is simply copied into the result list. 
-- Only elements with duplicates are transferred as (N E) lists.
data ListItem a = Multiple Int a
           | Single a
           deriving (Show, Eq)
encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified [] = []
encodeModified xs = map encode' (encode xs)
    where encode' (1, a) = Single a
          encode' (n, a) = Multiple n a
-- QuickCheck
prop_encodeModified = encodeModified "aaaabccaadeeee" == [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']


-- Q12 Decode a run-length encoded list
decodeModified :: Eq a => [ListItem a] -> [a]
decodeModified [] = []
decodeModified ((Single x):xs) = (x:decodeModified xs)
decodeModified ((Multiple n x):xs) = (replicate n x) ++ (decodeModified xs)
-- QuickCheck
prop_decodeModified xs = decodeModified (encodeModified xs) == xs


-- Q14 Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = (x:x:dupli xs)
-- QuickCheck
prop_dupli = dupli [1, 2, 3] == [1,1,2,2,3,3]


-- Q15 Replicate the elements of a list a given number of times. 
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = (replicate n x) ++ (repli xs n)
-- QuickCheck
prop_repli xs = repli xs 2 == dupli xs


-- Q16 Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery _ 0 = []
dropEvery xs n = dropEvery' xs (n-1) (n-1)
    where dropEvery' [] _ _ = []
          dropEvery' (x:xs) n 0 = dropEvery' xs n n
          dropEvery' (x:xs) n m = (x:dropEvery' xs n (m-1))
-- QuickCheck
prop_dropEvery (ListAndIndex xs n) = length (dropEvery xs n) == s - (div s n)
    where s = length xs


-- Q17 Split a list into two parts; the length of the first part is given.
split2 :: [a] -> Int -> ([a], [a])
split2 [] _ = ([], [])
split2 (x:xs) 1 = ([x], xs)
split2 (x:xs) n = ((x:a), b)
    where (a, b) = split2 xs (n-1) 
-- QuickCheck
prop_split2 = split2 "abcdefghik" 3 == ("abc", "defghik")


-- Q18 Extract a slice from a list.
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (x:xs) a b | a > 1 = slice xs (a-1) (b-1)
                 | b > 1 = (x:slice xs 1 (b-1))
                 | otherwise = [x]
-- QuickCheck
prop_slice = slice ['a','b','c','d','e','f','g','h','i','k'] 3 7 == "cdefg"


-- Q19 Rotate a list N places to the left. 
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs n | n == 0 = xs
            | n < 0 = rotate xs ((length xs) + n)
            | n > 0 = b ++ a
            where (a, b) = split2 xs n
-- QuickCheck
prop_rotate1 = rotate ['a','b','c','d','e','f','g','h'] 3 == "defghabc"
prop_rotate2 = rotate ['a','b','c','d','e','f','g','h'] (-2) == "ghabcdef"


-- Q20 Remove the K'th element from a list. 
removeAt :: [a] -> Int -> (a, [a])
removeAt [] _ = error "Index out of bounds"
removeAt (x:xs) 1 = (x, xs)
removeAt (x:xs) n = (a, x:b)
    where (a, b) = removeAt xs (n-1)
-- QuickCheck
prop_removeAt = removeAt "abcd" 2 == ('b',"acd")


-- Q21 Insert an element at a given position into a list. 
insertAt :: a -> [a] -> Int -> [a]
insertAt c xs 0 = error "Index out of bounds"
insertAt c [] _ = [c]
insertAt c xs 1 = (c:xs)
insertAt c (x:xs) n = (x:insertAt c xs (n-1))
-- QuickCheck
prop_insertAt = insertAt 'X' "abcd" 2 == "aXbcd"


-- Q22 Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range n m | n < m = (n:range (n+1) m)
          | otherwise = [m]
-- QuickCheck
prop_range = range 4 9 == [4,5,6,7,8,9]


-- Q23 Extract a given number of randomly selected elements from a list.
-- Example:
--   rnd_select "abcdefgh" 3 >>= putStrLn
--   eda
rnd_select :: [a] -> Int -> IO [a]
rnd_select [] _ = return []
rnd_select xs n = do 
    gen <- getStdGen  
    return $ take n [elementAt xs x | x <- randomRs (1, length xs) gen]
-- QuickCheck
-- prop_rnd_select = range 4 9 == [4,5,6,7,8,9]


-- Q24 Draw N different random numbers from the set 1..M. 
-- Example:
--   diff_select 6 49
--   [23,1,17,33,21,37]
--diff_select :: Int -> Int -> IO [Int]
--diff_select n m = return $ diff_select2 n [1..m] getStdGen
--    where diff_select2 n xs g = [elementAt xs x]
--        where x = randomR (1, length xs) g]


runTests = $quickCheckAll
