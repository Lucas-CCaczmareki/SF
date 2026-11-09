elemento :: Int -> [Int] -> Bool
elemento n [] = False
elemento n (x:xs) 
    | n == x = True
    | otherwise = elemento n xs
