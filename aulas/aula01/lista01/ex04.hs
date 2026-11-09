menorTres :: Int -> Int -> Int -> Int
menorTres x y z
    | x <= y && x <= z = x
    | y <= x && y <= z = y
    | z <= x && z <= y = z
