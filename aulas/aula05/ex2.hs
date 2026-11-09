data N = Zero | Suc N
    deriving(Eq, Show)

to_nat :: Int -> N
to_nat n
    | n == 0 = Zero
    | otherwise = Suc (to_nat (n - 1))