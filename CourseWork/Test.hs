digits :: Int -> [Int]
digits x = read (show x) :: [Int]

splitNum :: Int -> Int
splitNum x = read (take 2 (show x)) :: Int

split2Nums :: Int -> Int
split2Nums x = a `rem` b
    where
    (a,b) = (read (take 2 (show x)) :: Int, read (drop 2 (show x)) :: Int)

splits :: Int -> [Int] -> [([Int],[Int])]
splits x [] = []
splits x xs
    | x < length xs = splitAt x xs : splits (x+1) xs
    | otherwise = []

--infinate
splits2 :: Int -> [Int] -> [([Int],[Int])]
splits2 x [] = []
splits2 x xs = splitAt x xs : splits2 (x+1) xs

--O(n^4) combine x firsthalf x secondhalf 
splits3 :: [Int] -> [([Int],[Int])]
splits3 [a,b] = [([a],[b])]
splits3 xs = combine (firsthalf xs []) (secondhalf xs)

{-
Arguments: 
    a list of elements to calculate, 
    the current inner list (initially an empty list)

    - add the current element to the current inner list
    - add the current inner list to the outer list of lists
    - when there are two elements, return the final inner list
    - O(n^2) append x itself

Output: a list of lists
-}
firsthalf :: [Int] -> [Int] -> [[Int]]
firsthalf [] [] = [[]]
firsthalf [] ys = [[]]
firsthalf [x,z] ys = [ys ++ [x]]
firsthalf (x:xs) ys = 
    xys : firsthalf xs xys
    where xys = ys ++ [x]

{-
Argument: a list of numbers

  - add the tail of the list to the output
  - calculate the tail of the tail
  - when there are two elements left, give only the second
  - O(n) itself

Output: a list of lists
-}
secondhalf :: [Int] -> [[Int]]
secondhalf [] = [[]]
secondhalf [x,z] = [[z]]
secondhalf (x:xs) = xs : secondhalf xs

{-
Arguments: two lists of lists of equal length
Output: each corresponding list zipped togeather
O(n)
-}
combine :: [[Int]] -> [[Int]] -> [([Int],[Int])]
combine [] [] = []
combine [] ys = []
combine xs [] = []
combine (x:xs) (y:ys) = (x,y) : combine xs ys

main = do
    --print (splitNum 1234)
    --print (split2Nums 3612)
    --print (digits 1234)
    print "Hello"
    print (splits2 0 [1,2,3,4])