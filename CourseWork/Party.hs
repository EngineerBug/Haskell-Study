-- any imports go here

{-Begin Question 1.1-}
{-
Arguments: an integer 

    - (where f(x) = the input divided by 10)
    - adds the remainder from f(x) to a list
    - recurses on f(x) to the lower integer.

    Example: 1234
        digits (123) ++ [4]
        digits (12) ++ [3] ++ [4]
        digits (1) ++ [2] ++ [3, 4]
        digits (0) ++ [1] ++ [2, 3, 4]
        [] ++ [1, 2, 3, 4]
        output: [1, 2, 3, 4]
    
    The number in the () is f(x)
    The numbers in the [] are the remainders from each subsequent f(x)

Output: a list of single digit integers corresponding to the digits of the argument
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

digits :: Int -> [Int]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]
{-End Question 1.1-}


{-Begin Question 1.2-}
{-
Arguments: objects we are trying to find
            the list  we are searching

    - compare the input number to the first element of the input list
    - if they match:
        return false
    - else:
        recurse the function on the tail of the list
    - when the list is empty, there were no duplacates discovered

Output: booleon, if the list contains the number in question
-}
contains :: (Eq a) => a -> [a] -> Bool
contains y [] = False
contains y (x:xs)
    | y == x = True
    | otherwise = contains y xs

{-
Arguments: list of objects

    - if the list's tail "contain" its first element
        False
    - else:
        recurse the function on the tail of the list
    - when the list is empty, there were no duplacates discovered

Output: booleon, if the list doesn't contain duplicate objects
-}
isSet :: (Eq a) => [a] -> Bool
isSet [] = True
isSet (x:xs)
    | contains x xs = False
    | otherwise = isSet xs

{-
Arguments: an integer

Assumption: the input will be four digits in length

    - Runs three checks on the input

    - Are the first two digits divisible by the second two?
        take the number
        turn it into [Char]
        splits the [Char] in half
        read each result (independently) as an integer
        return (Int, Int)

    - Are there any duplicate digits?
        use the isSet function

    - Does the number contain a (0)
        0 is defined as non-possitive by the problem
        therefore using "contains" on 0 and xs can eliminate all non 

    - All three checks pass:
        True
    - Else:
        False

Output: weather the argument is a par.
-}
isPar :: Int -> Bool
isPar x
    | isSet xs && (b `rem` a == 0) && not (contains 0 xs) = True
    | otherwise = False
    where
        xs = digits x
        (a,b) = (read (take 2 (show x)) :: Int, read (drop 2 (show x)) :: Int)

{-
Arguments: none

    - generate a list of all four digit numbers
    - filter all results from the list through the "isPar" function

Output: a list of all numbers that meet the par criteria (should have a length of 44)
-}
pars :: [Int]
pars = filter isPar [1000..9999]
{-End Question 1.2-}


{-Begin Question 1.3-}
{-
arguments: a list of digits
    - note that while it is possible to input multi-digit numbers, the function follows the garbage-in-garbage-out ethic

    - use filter with the `elem` operator
    - compare a list of all positive digits with a compined list of the input values
    - Note: while this could include type variables, 
        it would need to pass in the digit domain as 
        an argument, which seems bad practice.

output: any positive digits (counting 0 as non-possitive) not contained in a or b
-}
setDifference :: [Int] -> [Int]
setDifference xs = filter (not . (`elem` xs)) [1,2,3,4,5,6,7,8,9]
{-
Arguments: a pair of integers

    - calculate any missing digits
        - use "setDifference" to find any missing digits
        - use pattern matching (x:xs) to get a missing digit and a list of any other missing digits
            - if there are any members of xs, (a,b) is not a Party
            - if xs is empty, x will be the required missing digit if (a,b)

    - test if both numbers are pars with "isPar"
    - test if the missing digit is a multiple of both inputs with `mod`.

Output: if the input is a Party!
-}
isParty :: (Int, Int) -> Bool
isParty (a,b)
    | isPar a && isPar b && null xs && a `mod` x == 0 && b `mod` x == 0 = True
    | otherwise = False
    where
        x:xs = setDifference (digits a ++ digits b)

makePairs :: [Int] -> [(Int,Int)]
makePairs [] = []
makePairs (x:xs) = map (\ y -> (x, y)) xs ++ makePairs xs

partys :: [(Int, Int)]
partys = filter isParty (makePairs pars)
{-End Question 1.3-}


-- any main functions for testing goes here
main :: IO ()
main = do
    --testing full domain of "digits"
    print (digits 1234567890) -- [1,2,3,4,5,6,7,8,9,0]

    --testing all facets of "isPar"
    print (isPar 2678)  -- True     test true case (from spec)
    print (isPar 3412)  -- False    test divisibility
    print (isPar 1111)  -- False    test duplication
    print (length pars) -- 44       test isPar validity

    --testing all functions involved in calculating "partys"
    print (setDifference [1,2,3,4,5,6,7,8]) -- [9]
    print (isParty (2754, 1836)) -- true (from spec)
    print (isParty (2754, 2754)) -- false
    print (makePairs [1,2,3])
    print partys