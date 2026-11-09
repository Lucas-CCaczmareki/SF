contaMaiores :: Int -> [Int] -> Int
contaMaiores n [] = 0


-- Guardas | funcia como um switch case pra chamada da função 
contaMaiores n (x:xs)
    | n < x = 1 + contaMaiores n xs
    | otherwise = contaMaiores n xs