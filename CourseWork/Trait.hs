import Data.List ( permutations )

isPrime :: Int -> Int -> Bool
isPrime x 1 = True
isPrime x y
    | x `mod` y == 0 = False
    | otherwise = isPrime x (y-1)

prime :: Int -> Bool
prime x = isPrime x (x-1)

number :: [Int] -> Int
number [] = 0
number (x:xs) = (10 ^ length xs * x) + number xs

possibles :: [[Int]]
possibles = permutations [1..9]

acceptable :: [Int] -> Bool
acceptable xs =
       a > 144 
    && a < 180 
    && b > 244 
    && b < 270 
    && c > 344 
    && c < 360 
    && not (prime a) 
    && not (prime b) 
    && not (prime c)
    where
        a = number (take 3 xs)
        b = number (drop 3 (take 6 xs))
        c = number (drop 6 xs)

trait ::[[Int]]
trait = filter acceptable possibles


main = do
    print (legnth trait)