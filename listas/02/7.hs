addFim :: Int -> [Int] -> [Int]
addFim n [] = n : []
addFim n (x:xs) = x : addFim n xs