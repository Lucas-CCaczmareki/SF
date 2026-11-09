verificaTriangulo :: Int -> Int -> Int -> Bool
verificaTriangulo x y z
    | x + z > y && x + y > z && y + z > x = True
    | otherwise = False