-- Semântica de expressões aritméticas
data E = Num Int | Soma E E | Mult E E 
    deriving (Eq, Show)

-- 3 * (4 + 5)
prog1 :: E
prog1 = Mult (Num 3) (Soma (Num 4) (Num 5))

-- (2*3) + (4*5)
prog2 :: E
prog2 = Soma (Mult (Num 2) (Num 3))  -- 1o arg do Soma
             (Mult (Num 4) (Num 5))  -- 2o arg do Soma

prog3 :: E
prog3 = Soma (Num 1) (Mult (Num 2) (Num 3))

bigStepE :: E -> Int
bigStepE (Num n) = n                                    -- se recebeu um nó da arvore sintática n, devolve um inteiro n
bigStepE (Soma e1 e2) = bigStepE e1 + bigStepE e2       -- se recebe um nó (soma entre 2), avalia a 1a parte, e a 2a parte (recursivamente) e soma os valores
bigStepE (Mult e1 e2) = bigStepE e1 * bigStepE e2       -- mesma coisa que o outro só que pra multiplicação