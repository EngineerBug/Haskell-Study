-- any imports go here
import Data.List ( permutations )

{-Begin Question 2.1-}
number :: [Int] -> Int
number [x] = x
number (x:xs) = (10 ^ length xs * x) + number xs

--old: number xs = read (concatMap show xs) :: Int
{-End Question 2.1-}

{-Begin Question 2.2-}
splits :: [Int] -> [([Int],[Int])]
splits [a,b] = [([a],[b])]
splits xs =  combine (firsthalf xs []) (secondhalf xs)

{-
Arguments: 
    a list of elements to calculate, 
    the current inner list (initially an empty list)

    - add the current element to the current inner list
    - add the current inner list to the outer list of lists
    - when there are two elements, return the final inner list

Output: a list of lists
-}
firsthalf :: [Int] -> [Int] -> [[Int]]
firsthalf [] [] = [[]]
firsthalf [x,z] ys = [ys ++ [x]]
firsthalf (x:xs) ys = (ys ++ [x]) : firsthalf xs (ys ++ [x])

{-
Argument: a list of numbers

  - add the tail of the list to the output
  - calculate the tail of the tail
  - when there are two elements left, give only the second

Output: a list of lists
-}
secondhalf :: [Int] -> [[Int]]
secondhalf [x,z] = [[z]]
secondhalf (x:xs) = xs : secondhalf xs

{-
Arguments: two lists of lists of equal length
Output: each corresponding list zipped togeather
-}
combine :: [[Int]] -> [[Int]] -> [([Int],[Int])]
combine [] [] = []
combine (x:xs) (y:ys) = (x,y) : combine xs ys
{-
Arguments:

    - Finds all 9! permutations of the digits [1..9]
    - uses "map" to apply "splits" to every permutation
        - this results in a list of lists of splits
    - concatenate all the lists together
    - leaving only a list of tuples

    Note: O(n!)

Output: a list of all possible multiplications involving all digits 1..9
-}
possibles :: [([Int], [Int])]
possibles = concatMap splits (permutations [1..9])

{-End Question 2.2-}

{-Begin Question 2.3-}
{-
Arguments: two lists of digits

    - turn both lists into numbers with "number"
    - multiply the numbers togeather
    - divide by ten until there is only one digit

Output: the remaining digit
-}
firstDigit :: [Int] -> [Int] -> Int -> Int
firstDigit as bs x
  | num < 10 = num
  | otherwise = num `div` 10
  where num = x `div` (10 ^ (length as + length bs - 2))

{-
Arguments: to lists of digits

    - turn both lists to numbers with "number"
    - compare the size of both numbers

Output: the smaller of the two lists
-}
shortest :: [Int] -> [Int] -> [Int]
shortest as bs
    | number as < number bs = as
    | otherwise = bs

{-
Arguments: two lists of numbers

    - turn both lists to numbers with "number"
    - multiply the numbers togeather
    - if the reversed number is the number, true

Output: true of the result is symmetric, otherwise false
-}
isPalendrome :: Int -> Bool
isPalendrome x
    | reverse num == num = True
    | otherwise = False
    where
        num = show x

{-
Arguments: a tuple of lists of digits

    - check if the multiplied number is symmetric
    - get the first digit of the multiplied number
        - check if it is 4
    - get the last digit of the smaller number
        - check if it is 3
    
Output: true of the pair is a possible solution
-}
isAcceptable :: ([Int],[Int]) -> Bool
isAcceptable (as,bs)
    | isPalendrome result && isFour == 4 && shortDigit == 3 = True
    | otherwise = False
    where
        result = number as * number bs
        isFour = firstDigit as bs result
        shortDigit = number (shortest as bs) `rem` 10

{-
Arguments: none

    - apply isAcceptable as a predicate to all permutations of [1..9]

Output: a list of all possible solutions
-}
acceptables :: [([Int],[Int])]
acceptables = filter isAcceptable possibles

{-End Question 2.3-}

-- any main functions for testing goes here

main :: IO ()
main = do
    print (number [9,1,2,4])    -- 9124 (from spec)
    print (length (splits [1..9]))
    print (firsthalf [1,2,3,4] [])
    print (secondhalf [1,2,3,4])
    -- print (isAcceptable ([7,1,6,3],[5,9,2,4,8])) -- True (from spec)
    -- print (isAcceptable ([7,6,1,3],[5,9,2,4,8])) -- False
    --print (splits [1..9])
    --print (length possibles)  -- 2903040 (from spec) (does run, but slowly)
    print (length acceptables)
        -- test1: 2m 53s
        -- test2: stack overflow because of a typo
        -- test3: 2m 47s
        -- test4: (on repl) 56s
        -- test5: (on repl) 55s
        -- test6: (repl, improve number) 44s
        -- test7: (repl, improve isAcceptable) 37s
        -- test8: (compile with ghc beforehand) 17s
        -- test9: (repl, update split) 25s
