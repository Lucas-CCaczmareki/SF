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
-- o operador de concatenação "++" concatena duas listas.

-- O primeiro elemento da lista é identificado como a cabeça(head) (x)
-- Os demais são a cauda (tail) (xs)
-- O padrão (x:xs) identifica uma lista, separando-a entre head e tail
somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs 