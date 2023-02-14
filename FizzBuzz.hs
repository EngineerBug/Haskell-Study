fizzBuzz :: Int -> [Char]
fizzBuzz x
    | x `mod` 3 == 0 && x `mod` 5 == 0 = "FizzBuzz"
    | x `mod` 3 == 0 = "Fizz"
    | x `mod` 5 == 0 = "Buzz"
    | otherwise = show x

fizzBuzzList :: [[Char]]
fizzBuzzList = map fizzBuzz [1..]

--this is a nice function, i want to keep it
--contains :: (Eq a) => a -> [a] -> Bool
--contains n (x:xs) = n == x || (xs /= [] && contains n xs)

main = do
    print(take 15 fizzBuzzList)
    print"Hi there, this is just to test the yml file."