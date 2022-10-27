-- any imports go here
import Data.List

{-Begin Question 2.1-}
number :: [Int] -> Int
number xs = read (concat (map (show) xs)) :: Int
{-End Question 2.1-}

{-Begin Question 2.2-}
splits :: [a] -> [([a],[a])]
splits [a,b] = [([a],[b])]
splits (x:xs) = ([x],(xs)):(zip (map (x:) (map fst (splits xs))) (map snd (splits xs)))

--possibles :: [([Int],[Int])]

{-End Question 2.2-}

{-Begin Question 2.3-}
--isAcceptable :: ([Int],[Int]) -> Bool

--acceptables :: [([Int],[Int])]

{-End Question 2.3-}

-- any main functions for testing goes here
main = do
    print (number [9,1,2,4])    -- 9124 (from spec)
    print (splits [1,2,3,4])

