data N = Zero | Suc N
    deriving(Eq, Show)

geq :: N -> N -> Bool
-- fazer essa comparação primeiro garante que o 2o n é 0. (se o 1o for zero, já cai no true de zero = zero)
geq n Zero = True
geq Zero n = False
geq (Suc n1) (Suc n2) = geq n1 n2 -- vai diminuindo até chegar nas comparaçoes eem zero

