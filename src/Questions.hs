module Sandbox where

-- Q11 Last element
tail2 :: [a] -> a
tail2 [x] = x
tail2 (_:xs) = (tail2 xs)


-- Q2 Last but one
butLast :: [a] -> a
butLast (x:[_]) = x
butLast (_:xs) = (butLast xs)


-- Q3 Element at
elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt [] _ = error "Index out of bounds"
elementAt (_:xs) n = elementAt xs (n-1)


-- Q4 List length
length2 :: [a] -> Int
length2 [] = 0
length2 (_:xs) = (length2 xs) + 1


-- Q5 reverse list
reverse2 :: [a] -> [a]
reverse2 xs = reverse3 xs []
    where reverse3 [x] ys = (x:ys)
          reverse3 (x:xs) ys = reverse3 xs (x:ys)


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


-- Q7 Flatten list
flatten :: [[a]] -> [a]
flatten [xs] = xs
flatten (x:xs) = x ++ flatten xs


-- Q8 Eliminate consecutive duplicates of list elements.
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs) | x == y = compress (y:xs)
                  | otherwise = (x:compress (y:xs))


-- Q9 Pack consecutive duplicates of list elements into sublists. 
-- If a list contains repeated elements they should be placed in separate sublists.
-- "aasddeee" -> ["aa", "s", "dd", "eee"]
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = reverse $ pack2 xs [x] []
    where pack2 [] ys zs = (ys:zs)
          pack2 (x:xs) (y:ys) zs | x == y = pack2 xs (x:y:ys) zs
                                 | otherwise = pack2 xs [x] ((y:ys):zs)


-- Q10  Run-length encoding of a list. 
-- Use the result of problem P09 to implement the so-called 
-- run-length encoding data compression method. 
-- Consecutive duplicates of elements are encoded as lists (N E) 
-- where N is the number of duplicates of the element E.
-- Example:
--   encode "aaaabccaadeeee"
--   [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode xs = [(length (y:ys), y) | (y:ys) <- pack xs]


-- Q11 Modified run-length encoding.
-- Modify the result of problem 10 in such a way 
-- that if an element has no duplicates it is simply copied into the result list. 
-- Only elements with duplicates are transferred as (N E) lists.
-- Example:
-- encodeModified "aaaabccaadeeee"
--   [Multiple 4 'a',Single 'b',Multiple 2 'c',
--   Multiple 2 'a',Single 'd',Multiple 4 'e']
data ListItem a = Multiple Int a
           | Single a
           deriving (Show, Eq)
encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified [] = []
encodeModified xs = map encode' (encode xs)
    where encode' (1, a) = Single a
          encode' (n, a) = Multiple n a

-- Q12 Decode a run-length encoded list
-- Example:
--  decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
--  "aaaabccaadeeee"
decodeModified :: Eq a => [ListItem a] -> [a]
decodeModified [] = []
decodeModified ((Single x):xs) = (x:decodeModified xs)
decodeModified ((Multiple n x):xs) = (replicate n x) ++ (decodeModified xs)


-- Q14 Duplicate the elements of a list.
-- Example: 
--  dupli [1, 2, 3]
--  [1,1,2,2,3,3]
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = (x:x:dupli xs)

-- Q15 Replicate the elements of a list a given number of times. 
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = (replicate n x) ++ (repli xs n)


-- Q16 Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery _ 0 = []
dropEvery xs n = dropEvery' xs (n-1) (n-1)
    where dropEvery' [] _ _ = []
          dropEvery' (x:xs) n 0 = dropEvery' xs n n
          dropEvery' (x:xs) n m = (x:dropEvery' xs n (m-1))


-- Q17 Split a list into two parts; the length of the first part is given.
-- Example:
--   split "abcdefghik" 3
--   ("abc", "defghik")
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split (x:xs) 1 = ([x], xs)
split (x:xs) n = ((x:a), b)
    where (a, b) = split xs (n-1) 


-- Q18 Extract a slice from a list.
-- Example:
--   slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
--   "cdefg"
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (x:xs) a b | a > 1 = slice xs (a-1) (b-1)
                 | b > 1 = (x:slice xs 1 (b-1))
                 | otherwise = [x]


-- Q19 Rotate a list N places to the left. 
-- Examples:
--   rotate ['a','b','c','d','e','f','g','h'] 3
--   "defghabc"
--   rotate ['a','b','c','d','e','f','g','h'] (-2)
--   "ghabcdef"
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs n | n == 0 = xs
            | n < 0 = rotate xs ((length xs) + n)
            | n > 0 = b ++ a
            where (a, b) = split xs n


-- Q20 Remove the K'th element from a list. 
-- Example:
--   removeAt 2 "abcd"
--   ('b',"acd")
removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error "Index out of bounds"
removeAt 1 (x:xs) = (x, xs)
removeAt n (x:xs) = (a, x:b)
    where (a, b) = removeAt (n-1) xs


-- Q21 Insert an element at a given position into a list. 
-- Example:
--   insertAt 'X' "abcd" 2
--   "aXbcd"
insertAt :: a -> [a] -> Int -> [a]
insertAt c xs 0 = error "Index out of bounds"
insertAt c [] _ = [c]
insertAt c xs 1 = (c:xs)
insertAt c (x:xs) n = (x:insertAt c xs (n-1))


-- Q22 Create a list containing all integers within a given range.
-- Example:
--   range 4 9
--   [4,5,6,7,8,9]
range :: Int -> Int -> [Int]
range n m | n < m = (n:range (n+1) m)
          | otherwise = [m]


-- Q23 Extract a given number of randomly selected elements from a list.
-- Example:
--   rnd_select "abcdefgh" 3 >>= putStrLn
--   eda

