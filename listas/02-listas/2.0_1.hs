multLista :: Int -> [Int] -> [Int]
multLista n [] =  []
-- retorna uma nova lista através do operador cons(:) de construção
multLista n (x:xs) = n * x : multLista n xs