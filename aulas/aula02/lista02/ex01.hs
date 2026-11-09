multLista :: Int -> [Int] -> [Int]
multLista n [] = []
multLista n (x:xs) = x*n : multLista n xs
