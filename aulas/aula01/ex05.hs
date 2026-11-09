expoente :: Int -> Int -> Int
expoente n 0 = 1
expoente n 1 = n
expoente 0 n = 0
expoente n m = n * expoente n (m-1)