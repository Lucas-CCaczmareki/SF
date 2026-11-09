geraLista :: Int -> Int -> [Int]
geraLista 0 n = []
geraLista m n
    | m > 0 = n:geraLista (m-1) n
    -- | otherwise =  [] acho que isso não precisa

