data N = Zero | Suc N
    deriving(Eq, Show)

to_int :: N -> Int
to_int Zero = 0
to_int (Suc n1) = 1 + to_int(n1)