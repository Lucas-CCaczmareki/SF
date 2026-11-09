-- exemplo de recursão em haskell pra calcular um fatorial
fat :: Int -> Int
fat 0 = 1
fat 1 = 1
fat n = n * fat (n-1)