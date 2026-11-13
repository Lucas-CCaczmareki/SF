
-- Definição das árvore sintática para representação dos programas:

-- Linguagem de expressões aritméticas
data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
      |Div E E
   deriving(Eq,Show)

-- Linguagem de expressões booleanas
data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E    -- menor ou igual
      | Igual E E  -- verifica se duas expressões aritméticas são iguais
   deriving(Eq,Show)

-- Linguagem imperativa
data C = While B C
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
    | SevenTimes C          -- Executa o comando C 7 vezes
    | DoWhile C B           -- Do C While B: executa C enquanto B avalie para verdadeiro
    | DuplaAtrib E E E E    -- recebe 2 variáveis e 2 expressões (DuplaATrib (Var v1) (Var v2) e1 e2) e faz v1:=e1 e v2:=e2
    | AtribCond B E E E     -- AtribCond b (Var v1) e1 e2: se b for verdade, então faz v1:e1, se B for falso faz v1:=e2
    | Swap E E              -- swap(x,y): troca o conteúdo das variáveis x e y 
    | Unless B C C          -- Unless B C1 C2: se B avalia para falso, então executa C1, caso contrário, executa C2
    | Assert B C            -- caso B avalie para True então executa C. Caso contrário, não faz nada
   deriving(Eq,Show)                


----------------------------------------------------------------------

--    As próximas funções, servem para manipular a memória (sigma)

----------------------------------------------------------------------


-- A próxima linha de código diz que o tipo memória é equivalente a uma lista de tuplas, onde o
-- primeiro elemento da tupla é uma String (nome da variável) e o segundo um Inteiro (conteúdo da variável):
type Memoria = [(String,Int)]

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp", 0), ("y", 0)]

-- -----------------------------------------------------------------------------------------------------------
-- A função procuraVar recebe uma memória, o nome de uma variável e 
-- retorna o conteúdo dessa variável na memória. 

-- Exemplo:
-- *Main> procuraVar exSigma "x"
-- 10

procuraVar :: Memoria -> String -> Int
-- se recebe uma lista vazia, retorna erro. (Se não achou a variável com esse nome tb)
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado") 
-- (s,i) = x. O 1o elemento da lista, divido num par (String, Int).
-- xs é a cauda da lista. v é o nome (String) da variável
procuraVar ((s,i):xs) v
  | s == v     = i
  | otherwise  = procuraVar xs v -- procura recursivamente (itera sobre) na lista o nome da variável
-- -----------------------------------------------------------------------------------------------------------

-- -----------------------------------------------------------------------------------------------------------
-- A função mudaVar, recebe uma memória, o nome de uma variável e um novo conteúdo para essa
-- variável e devolve uma nova memória modificada com a varíável contendo o novo conteúdo. 

-- Exemplo:
-- A chamada. Essa chamada é equivalente a operação exSigma[temp->20]:
-- *Main> mudaVar exSigma "temp" 20
-- [("x",10),("temp",20),("y",0)]

mudaVar :: Memoria -> String -> Int -> Memoria
mudaVar [] v n = error ("Variavel " ++ v ++ " nao definida no estado") -- [] mem. v nome da var. n novo conteudo da var.
mudaVar ((s,i):xs) v n
  | s == v     = ((s,n):xs)             -- troca i (valor atual) por n e concatena com a cauda
  | otherwise  = (s,i): mudaVar xs v n  -- mantém i (val atual) e concatena com chamada recursiva que percorre a cauda buscando o nome da variável
-- -----------------------------------------------------------------------------------------------------------


-- ----------------------------------------------------------
--                         TRABALHO:
--- Completar os casos comentados das seguintes funções:
-- ----------------------------------------------------------

-- semântica de expressões aritméticas (bigStep)
ebigStep :: (E, Memoria) -> Int
ebigStep (Var x, mem)       = procuraVar mem x  -- avalia a variável e retorna o valor dela
ebigStep (Num n, mem)       = n                 -- avalia o número e retorna o próprio número
-- faz a avaliação da expressão (vai indo recursivamente até chegar num único numero) e aplica a operação com a avaliação recursiva do outro lado
ebigStep (Soma e1 e2, mem)  = ebigStep (e1, mem) + ebigStep (e2, mem) 
ebigStep (Sub e1 e2, mem)   = ebigStep (e1, mem) - ebigStep (e2, mem)
ebigStep (Mult e1 e2, mem)  = ebigStep (e1, mem) * ebigStep (e2, mem)
ebigStep (Div e1 e2, mem)   = ebigStep (e1,mem) `div` ebigStep (e2,mem) -- usa o `div` que é a divisão inteira. Descarta a vírgula

-- semântica de expressões booleanas (bigStep)
bbigStep :: (B, Memoria) -> Bool
bbigStep (TRUE, mem)  = True
bbigStep (FALSE, mem) = False

bbigStep (Not b, mem) 
    | bbigStep (b, mem)      == True  = False
    | otherwise                       = True 

bbigStep (And b1 b2, mem)
    -- avaliação em curto circuito, se o 1o n for true, retorna false direto.
    | bbigStep (b1, mem) == True = bbigStep(b2, mem)
    | otherwise = False 

bbigStep (Or b1 b2, mem)
    -- mesma ideia da avaliação em curto circuito. Se o 1o é false, retorna o valor do outro
    | bbigStep(b1, mem) == False = bbigStep(b2, mem)
    | otherwise = True

bbigStep (Leq e1 e2, mem)
    -- avalia as 2 expressões e compara os valores
    | ebigStep (e1, mem) <= ebigStep(e2, mem) = True -- usa o <= do próprio haskell
    | otherwise = False

bbigStep (Igual e1 e2, mem)
    -- avalia as 2 expressões e compara os valores
    | ebigStep (e1, mem) == ebigStep(e2, mem) = True
    | otherwise = False

-- semântica de liguagem imperativa
cbigStep :: (C, Memoria) -> (C, Memoria)
cbigStep (Skip, mem) = (Skip, mem)

cbigStep (If b c1 c2, mem)
    | bbigStep (b, mem) == True = cbigStep(c1, mem) -- se true avalia comando 1
    | otherwise = cbigStep(c2, mem)                 -- se false, avalia comando 2

cbigStep (Seq c1 c2, mem) =
    -- let/in vai forçar a execução na ordem desejada. Primeiro C1 e depois C2
    -- let avalia o primeiro comando da sequência com a memória inicial mem
    -- retorna (Skip, mem1). Usamos o _ pra descartar o skip
    let (_, mem1) = cbigStep(c1, mem)
    in cbigStep (c2, mem1) -- agora valia o C2 com a memória modificada pelo C1

-- Var x onde x é o nome da variável (String)
-- E onde E representa o número (ou expressão numérica) cujo valor vai ser atribuído à variável
-- mudaVar recebe mem, str, val. Val é a valiação de e com a memória atual
cbigStep (Atrib (Var x) e, mem) = (Skip, (mudaVar mem x (ebigStep (e, mem))))

cbigStep (While b c, mem)
-- 1o avalia se (b, mem) é True. 
-- Depois uma sequência de comandos o C : While B do C e a memória.
    | bbigStep (b, mem) == True = cbigStep(Seq c (While b c), mem)
    | otherwise = (Skip, mem) -- quando resulta em false, não executa comandos e retorna memória

cbigStep (SevenTimes c, mem) = cbigStep(Seq c (Seq c (Seq c (Seq c (Seq c (Seq c c))))), mem)

cbigStep (DoWhile c b, mem) = cbigStep(Seq c (While b c), mem)

cbigStep (DuplaAtrib (Var v1) (Var v2) e1 e2, mem) =
   let n1 = ebigStep(e1, mem) -- avalia as expressões primeiro e guarda em n1/n2
       n2 = ebigStep(e2, mem)
   in (Skip, (mudaVar(mudaVar mem v1 n1) v2 n2)) -- isso é oq o cbigStep retorna quando recebe uma DuplaATrib

-- ex prog: cbigStep (AtribCond FALSE (Var "x") (Mult(Num 2)(Num 2)) (Mult(Num 2)(Num 4)), exSigma)
-- return: (Skip,[("x",8),("temp",0),("y",0)]). OK
cbigStep (AtribCond b v1 e1 e2, mem) -- v1 fica como E(expressão) por que Atrib espera receber um E
    | bbigStep (b, mem) == True = cbigStep(Atrib v1 e1, mem) -- avalia b, se for True, chama função de Atrib com e1
    | otherwise = cbigStep(Atrib v1 e2, mem) -- se for false chama com e2

-- ex prog: cbigStep (Swap (Var "x") (Var "y"), exSigma)
-- saida: (Skip,[("x",0),("temp",0),("y",10)])
cbigStep (Swap (Var v1) (Var v2), mem) =
    let temp_v2 = procuraVar mem v2 -- guarda em temp_v2 o valor de v2 com a memória atual (sem modificações)
    -- aplica um mudaVar na memória modificada por outro mudaVar
        -- o mudaVar interno procura o valor de v1 na memória sem modificação e coloca em v2
    -- o mudaVar externo, alterna a memória de v1, colocando o valor original de v2 lá
    in (Skip, (mudaVar (mudaVar mem v2 (procuraVar mem v1)) v1 temp_v2))

-- ex prog: cbigStep (Unless TRUE (Swap (Var "x") (Var "y")) (AtribCond FALSE (Var "x") (Mult (Num 2) (Num 2)) (Mult (Num 2) (Num 4))), exSigma)
-- saida: (Skip,[("x",8),("temp",0),("y",0)])
cbigStep (Unless b c1 c2, mem)
    | bbigStep(b, mem) == False = cbigStep(c1, mem) -- se avaliar pra V, executa c1
    | otherwise = cbigStep(c2, mem)                 -- se for F, executa c2

-- ex prog: cbigStep (Assert TRUE (Swap (Var "x") (Var "y")), exSigma)
-- saida: (Skip,[("x",0),("temp",0),("y",10)])
cbigStep (Assert b c, mem)
    | bbigStep(b, mem) == True = cbigStep(c, mem)   -- se for true executa
    | otherwise = cbigStep(Skip, mem)               -- se for false n faz nada


exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]

exSigma3 :: Memoria
exSigma3 = [("x", 1), ("y", 2), ("z", 3)]
--------------------------------------
---
--- Exemplos de programas para teste
---
--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS DE PROGRAMAS QUE USEM TODOS OS COMANDOS NOVOS:

-- * SevenTimes
-- Incrementa 1 sete vezes na variável "x"
prog7x :: C
prog7x = (SevenTimes (Atrib (Var "x") (Soma (Var "x") (Num 1))))

-- * DoWhile
progDoWhile :: C
progDoWhile = (DoWhile (Seq 
                        (Atrib (Var "temp") (Sub (Var "temp") (Num 10)))    -- drecrementa de 10 em 10
                        (Atrib (Var "y") (Soma (Var "y") (Num 1))))         -- incrementa de 1 em 1 
                        (Leq (Var "y") (Num 4)))                            -- repete os 2 comandos da sequência enquanto y <= 4

-- * DuplaAtribuicao
prog2Atrib :: C
prog2Atrib = (DuplaAtrib (Var "temp") (Var "y")
              (Div (Mult (Var "x") (Num 5)) (Num 2)) -- 1a expr: x(10) * 5 / 2
              (Mult (Num 2) (Var "temp"))) -- 2a expr: 2 * temp(0) pq ainda não atribuiu o 25 de cima.

-- * AtribCond
progAtribCond :: C
progAtribCond = (AtribCond (Leq (Var "x") (Num 20)) -- booleano x(10) <= 20
                           (Var "temp")  -- variavel pra onde vai o resultado da expressao
                           (Soma (Num 5) (Num 8))   -- e1, aparece quando true
                           (Div (Var "x") (Num 3))) -- e2, aparece quando false

-- * Swap 
progSwap :: C
progSwap = (Seq 
             (Atrib (Var "x") (Soma (Var "x") (Mult (Var "z") (Num 2)))) -- considerando sigma3 = x(1) + 2 * 3 = 7
             (Swap (Var "x") (Var "y"))) -- considerando sigma 3 y = 2 troca com x = 7

-- * Unless
progUnless :: C
progUnless = (Unless (Not (Igual (Var "x") (Var "y"))) -- (x != y) = False -> Not False -> True 
                     (Atrib (Var "x") (Num 100))
                     (Atrib (Var "x") (Num 67))) -- esse que vai ser executado, se a entrada for exSigma3

-- * Assert
progAssert1 = (Assert (TRUE) --  caso dê verdadeiro
                     (prog7x))
progAssert2 = (Assert (FALSE) -- caso dê falso
                     (prog7x))
-------------------------------------


---
--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse
--- programa já é possível rodar com a implementação inicial  fornecida:

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

---
--- para rodar:
-- *Main> ebigStep (progExp1, exSigma)
-- 13
-- *Main> ebigStep (progExp1, exSigma2)
-- 6

--- Para rodar os próximos programas é necessário primeiro implementar as regras da semântica
---


---
--- Exemplos de expressões booleanas:
teste1 :: B
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))

teste2 :: B
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))


---
-- Exemplos de Programas Imperativos:

testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
               (Atrib (Var "y") (Var "z")))

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))