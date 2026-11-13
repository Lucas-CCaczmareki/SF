data Nat = Zero | Suc Nat
    deriving(Eq, Show)

to_int :: Nat -> Int
to_int Zero = 0               -- se recebe um zero, devolve 0
to_int (Suc n) = 1 + to_int n -- se recebe algum Nat q tem Suc, devolve 1 + aquele nat sem o Suc (que pode ser outra sequencia de sucs ou Zero)

