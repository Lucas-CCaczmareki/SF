maiores :: Int -> [Int] -> [Int]
maiores n [] = []
maiores n (x:xs)
    | x > n = x : [] ++ maiores n xs
    | otherwise = maiores n xs