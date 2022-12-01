digits :: Int -> [Int]
digits x = read (show x) :: [Int]

splitNum :: Int -> Int
splitNum x = read (take 2 (show x)) :: Int

split2Nums :: Int -> Int
split2Nums x = a `rem` b
    where
    (a,b) = (read (take 2 (show x)) :: Int, read (drop 2 (show x)) :: Int)

main = do
    --print (splitNum 1234)
    --print (split2Nums 3612)
    --print (digits 1234)
    print (reverse [1,2,3,4])