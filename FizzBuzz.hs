fizzBuzz :: Int -> [Char]
fizzBuzz x
    | x `mod` 3 == 0 && x `mod` 5 == 0 = "FizzBuzz"
    | x `mod` 3 == 0 = "Fizz"
    | x `mod` 5 == 0 = "Buzz"
    | otherwise = show x

fizzBuzzList :: [[Char]]
fizzBuzzList = map (fizzBuzz) [1..]

main = do
    print(take 15 fizzBuzzList)