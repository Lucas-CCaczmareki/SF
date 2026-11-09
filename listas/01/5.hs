potencia :: Int -> Int -> Int
potencia n 0 = 1
potencia n 1 = n
potencia n m = n * potencia n (m-1)