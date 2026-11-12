-- func q verifica se a soma de 2 lados quaisquer é sempre maior q o terceiro lado
-- propriedade dos triangulos
verifyTriangle :: Int -> Int -> Int -> Bool
verifyTriangle a b c
    -- aparentemente guards aqui eu que defino o resultado de retorno. Ent n da pra mandar a própria expressão sem que copiar ela de novo
    | a + b > c  && a + c > b && c + b > a  = True
    | otherwise                             = False

-- jeito retornando direto o resultado da comparação
-- VerifyTriangle a b c =  a + b > c  && a + c > b && c + b > a


