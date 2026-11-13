import Distribution.SPDX (LicenseId(Caldera_no_preamble))
-- iniciar interpretador GHCi: "ghci"
-- fechar interpretador GHCi: ":q"
-- carregar programas no interpretador: ":l nome_do_arquivo.hs"

-- operadores lógicos e aritméticos definidos pela linguagem:
-- +, -, *, /
-- && (AND), || (OR), not (negação)
-- == (igual), /= (diferente), >, <, >=, <=

-- definição de funções
-- exemplo 1
min_n :: Int -> Int -> Int    -- nome :: tipo da entrada(In) -> In -> ... -> Out
min_n a b                     -- define os nomes que os argumentos de entrada recebem (Os 2 primeiros Int -> Int)
    
    -- | definem guardas. São uma espécie de if / else if / else
    -- | condição = resultado

    | a <= b  = a   -- se a que min recebeu é <= b, retorna a como resultado
    | otherwise = b -- else retorna b

-- exemplo 2
palindromo :: String -> Bool
-- func in = out
palindromo s = s == reverse s 

-- exemplo 3
fat :: Int -> Int
-- retornano valores específicos pra entradas específicas
fat 0 = 1
fat 1 = 1
-- retornando uma chamada recursiva da função para simular loop
fat n = n * fat (n-1)

-- Listas = estrutura de dados pra guardar valores do mesmo tipo
-- sintaxe: [True, False, False]
-- colchetes delimita, vírgula separa.
-- ex: String é uma lista de caracteres: ['L', 'u', ..., 's']. "abc" == ['a', 'b', 'c'] retorna True

-- o operador ":" chamado de cons insere um elemento na frente da lista
-- O operador ":" constrói uma listaa

-- O primeiro elemento da lista é identificado como a cabeça(head) (x)
-- Os demais são a cauda (tail) (xs)
-- O padrão (x:xs) identifica uma lista, separando-a entre head e tail
somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs 

-- o operador de concatenação "++" concatena duas listas.
join :: [Int] -> [Int] -> [Int]
join l1 l2 = l1 ++ l2


-- Algoritmo para ordenação de listas
-- divide-se o algoritmo em 2 passos:
    -- 1: uma recursão que insere o elemento na posição correta e ordena a cauda da lista
    -- 2: o algoritmo de inserção, que coloca o número na sua posição correta

-- o algoritmo de inserção vai sempre receber um numero, uma lista, e devolver OUTRA lista
ins :: Int -> [Int] -> [Int]
ins a [] = [a] -- se recebe o numemro e lista vazia, insere.
ins a (x:xs)
    -- se o a for menor q o primeiro da lista, coloca ele lá
    | a <= x = a : x : xs
    | otherwise = x : ins a xs

-- recebe uma instância de lista, e retorna a mesma ordenada
iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x (iSort xs) -- passa a cauda ordenada também.

-- uma lista pode ser recebida através de vários formatos diferentes
-- x:xs, _:xs, x:_, [x] e []
-- o _ significa que tu descarta uma parte

-- tem funções já do haskell pra esse primeiro (head) e pro último (last)
primeiro :: [Int] -> Int
primeiro [] = error "lista vazia!"
primeiro (x:_) = x -- recebe a lista, divide em head:tail e descarta(_) a tail

ultimo :: [Int] -> Int
ultimo [] = error "lista vazia!"
ultimo [x] = x -- se for uma lista de um elemento, retorna o elemento
ultimo (_:xs) = ultimo xs -- vai recursivamente até o último elemento

maiorEmenor :: [Int] -> (Int, Int)
maiorEmenor xs = (ultimo ordenada, primeiro ordenada)
    -- O where, serve pra nessa equação local (o maiorEmenor) dar um nome
    -- pra uma equação que vai ser reutilizada várias vezes
    where ordenada = iSort xs

-- tipos algébricos: são usados para modelar coisas que não são facilmente representadas
-- usando os tipos básicos. Como grafos e árvores.

-- são descritos através da enumeração de elementos:
-- os valores Frio e Calor são os construtores do tipo temperatura
-- nomes de tipos e construtores começam com letras maiusculas
data Temperatura = Frio | Calor
    -- deriving é um atalho pra declarar quais capacidades o nosso tipo tem
    -- eq permite comparar igualdade, e show permite ser impresso como string, por exemplo.
    deriving(Eq, Show)


data Estacao = Verao | Outono | Inverno | Primavera
    deriving(Eq, Show)

-- funcao pra avaliar nossos tipos.
tempo :: Estacao -> Temperatura
tempo x
    | x == Verao = Calor
    | otherwise = Frio

-- Os construtores do tipo são os valores que eles podem assumir
-- podem ser os criados ou os básicos
data Forma = Circulo Float | Retangulo Float Float
    deriving(Eq, Show)

area :: Forma -> Float
area (Circulo r) = pi * r * r -- se o forma receber um Circulo Float
area (Retangulo b a) = b * a -- se o forma receber um Retangulo Float Float

-- Os tipos algébricos também podem ser recursivos
-- o tipo pode ser uma Folha Int 
-- ou um Nodo Int com duas árvores (que podem ser folhas)
data Arvore = Folha Int | Nodo Int Arvore Arvore
    deriving(Eq, Show)

-- arv1 :: Arvore
arv1 = Nodo 10 (Nodo 14 (Nodo 1 (Folha 4) (Folha 2)) (Folha 6)) (Folha 9)
-- Nodo Int (10) Arv(...) Arv(...)

-- exemplo de função usando um tipo algébrico recursivo
somaArvore :: Arvore -> Int
somaArvore (Folha n) = n
somaArvore (Nodo n a1 a2) = n + somaArvore a1 + somaArvore a2

-- implementando os numeros naturais de semantica formal em haskell
data N = Zero | Suc N
    deriving(Eq, Show)

soma :: N -> N -> N
soma Zero n = n                     -- n1 = Zero e n com todos os sucs. retorna
soma (Suc n1) n2 = soma n1 (Suc n2) -- tirar um suc do n1 e colocar no n2 até o n1 ser zero

-- A semântica operacional descreve a semântica de uma linguagem através 
-- da descrição de um interpretador/maquina virtual.

-- BIG STEP
-- ↓: E → N
-- recebe uma expressão e devolve o resultado da avaliação

-- em haskell podemos usar tipos algébricos para construir a linguagem
-- LINGUAGEM DE EXPRESSÕES ARTIMÉTICAS
-- E:= n | E + E | E * E vira
data E = Num Int | Soma E E | Mult E E
    deriving(Eq, Show)

-- exemplo: 1 + 2 * 3
prog1 :: E
prog1 = Soma (Num 1) (Mult (Num 2) (Num 3))

-- exemplo 2: (2*3)+(4*5)
prog2 :: E
prog2 = Soma (Mult (Num 2) (Num 3))
             (Mult (Num 4) (Num 5))

-- implementação da função semântica
bigStepE :: E -> Int -- E é nosso tipo algébrico que representa a linguagem de expressões aritméticas
-- Semânticas
bigStepE (Num n) = n                                    -- semântica NUM
bigStepE (Soma e1 e2) = (bigStepE e1) + (bigStepE e2)   -- semântica SOMA
bigStepE (Mult e1 e2) = (bigStepE e1) * (bigStepE e2)   -- semântica MULT

-- LINGUAGEM DE EXPRESSÕES BOOLEANAS
data B = TRUE | FALSE | Not B | And B B | Or B B
    deriving (Eq, Show)

-- exemplo: TRUE && FALSE || TRUE
prog3 :: B
prog3 = Or (And TRUE FALSE) TRUE

-- exemplo: TRUE && FALSE || FALSE && TRUE
prog4 :: B
prog4 = Or (And TRUE FALSE) (And FALSE TRUE)

bigStepB :: B -> Bool
bigStepB TRUE = True
bigStepB FALSE = False
bigStepB (Not b)
    | bigStepB b == True = False
    | otherwise = True
bigStepB (And b1 b2)
-- avaliação em curto circuto, se a 1a é falsa já retorna falso
-- se não, o resultado do E é igual ao resultado da segunda, retorna ela
    | bigStepB b1 == False = False
    | otherwise = bigStepB b2
-- mesma ideia da avaliação em curto circuito adaptada pro OR
-- um true leva tudo à true, se o 1o é false, o resultado é igual à segunda
bigStepB (Or b1 b2)
    | bigStepB b1 == True = True
    | otherwise = bigStepB b2