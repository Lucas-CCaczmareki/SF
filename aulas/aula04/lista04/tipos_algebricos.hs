-- Isso aqui faz alguma coisa q eu n faço ideia do que é
-- Tem alguma coisa a ver com definição de tipos algébricos

data Temperatura = Frio | Calor
    deriving (Eq, Show)

data Estacao = Verao | Outono | Inverno | Primavera
    deriving (Eq, Show)

data Forma = Circulo Float | Retangulo Float Float
    deriving (Eq, Show)

tempo :: Estacao -> Temperatura
tempo Verao = Calor
tempo _ = Frio

redondo :: Forma -> Bool
redondo (Circulo x) = True
redondo (Retangulo x y) = False

area :: Forma -> Float
area (Circulo r) = pi * r * r
area (Retangulo b a) = b * a