-- any imports go here
import Data.List ( permutations )

{-Begin Question 2.1-}
number :: [Int] -> Int
number xs = read (concatMap show xs) :: Int
{-End Question 2.1-}

{-Begin Question 2.2-}
splits :: [a] -> [([a],[a])]
splits [] = []
splits [a,b] = [([a],[b])]
splits (x:xs) = ([x],xs):zip (map ((x:) . fst) (splits xs)) (map snd (splits xs))

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
shortest :: ([Int], [Int]) -> [Int]
shortest (as,bs)
    | number as < number bs = as
    | otherwise = bs

isPalendrome :: ([Int],[Int]) -> Bool
isPalendrome (as,bs)
    | evenOrOddLength == 0 && reverse (take mid num) == drop mid num = True
    | evenOrOddLength == 1 && reverse (take mid num) == drop (mid+1) num = True
    | otherwise = False
    where
        num = show (number as * number bs)
        evenOrOddLength = length num `mod` 2
        mid = length num `div` 2
{-
isPalendrome :: ([Int],[Int]) -> Bool
isPalendrome (as,bs)
    | reverse (num) == num = True
    | otherwise = False
    where
        num = show (number as * number bs)
-}

isAcceptable :: ([Int],[Int]) -> Bool
isAcceptable (as,bs)
    | isPalendrome (as,bs) && head num == '4' && last short == 3 = True
    | otherwise = False
    where
        num = show (number as * number bs)
        short = shortest (as, bs)


acceptables :: [([Int],[Int])]
acceptables = filter isAcceptable possibles
{-End Question 2.3-}

-- any main functions for testing goes here

main :: IO ()
main = do
    print (number [9,1,2,4])    -- 9124 (from spec)
    print (length (splits [1..9]))
    print (isAcceptable ([7,1,6,3], [5,9,2,4,8])) -- True (from spec)
    print (isAcceptable ([7,6,1,3], [5,9,2,4,8])) -- False
    --print (splits [1..9])
    --print (length possibles)  -- 2903040 (from spec) (does run, but slowly)
    --print (length acceptables)  
        -- test1: 2m 53s
        -- test2: stack overflow because of a typo
        -- test3: 2m 47s
