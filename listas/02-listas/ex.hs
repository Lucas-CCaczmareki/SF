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

ins_rep :: Int -> [Int] -> [Int]
ins_rep a [] = [a]
ins_rep a (x:xs)
    | a == x = ins x xs
    | otherwise = a : x : xs 

-- exercício exemplo 1: achar o menor e o maior da lista com iSort

maiorEmenor :: [Int] -> (Int, Int) -- recebe uma lista e devolve um par?
maiorEmenor [] = error "lista vazia!" -- isso aqui protege o head, mas o warning ainda não some
maiorEmenor xs =  (last ordenada, head ordenada) -- o head causa um warning por que ele não trata lista vazia
    where ordenada = iSort xs -- o where serve pra dar um nome pra uma expressão que vai reutilizada várias vezes no mesmo código

-- também dá pra fazer esse exercício assim pra n ter o warning, mas é literalmente refazer o head e o last
-- usando casamento de padrão, os padrões de formato das listas são
-- x:xs, _:xs, x:_, [x] e []

primeiro :: [Int] -> Int
primeiro [] = error "lista vazia!"
primeiro (x:_) = x -- recebe a lista, divide em head:tail e descarta(_) a tail

ultimo :: [Int] -> Int
ultimo [] = error "lista vazia!"
ultimo [x] = x -- se for uma lista de um elemento, retorna o elemento
ultimo (_:xs) = ultimo xs -- vai recursivamente até o último elemento

maiorEmenor2 :: [Int] -> (Int, Int)
maiorEmenor2 xs = (ultimo ordenada, primeiro ordenada)
    where ordenada = iSort xs


-- muda o comportamento mudando o ins
-- recebe uma instância de lista, e retorna a mesma ordenada
iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins_rep x (iSort xs) -- passa a cauda ordenada também.