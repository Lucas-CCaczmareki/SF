data N = Zero | Suc N
    deriving(Eq, Show)

-- recebe 2 inteiros e devolve 1
soma :: N -> N -> N
soma Zero n = n
soma (Suc n1) n2 = soma n1 (Suc n2)

