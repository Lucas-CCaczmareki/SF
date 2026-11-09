data N = Zero | Suc N
    deriving(Eq, Show)

-- recebe 2 naturais e devolve 1
soma :: N -> N -> N
soma Zero n = n
soma (Suc n1) n2 = soma n1 (Suc n2)

-- recebe 2 naturais e devolve 1
mult :: N -> N -> N
mult Zero n = Zero
-- ex: 2 * 2
-- o 2 = Suc n1, então n1 é igual a 1, e isso vai sendo diminuido até chegar em 0
mult (Suc n1) n2 = soma (mult n1 n2) n2