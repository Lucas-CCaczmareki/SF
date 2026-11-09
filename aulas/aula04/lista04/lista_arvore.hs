data Arvore = Folha Int | Nodo Int Arvore Arvore
    deriving(Eq, Show)


arv :: Arvore
arv = Nodo 7 (Nodo 2 (Folha 4) (Folha 3)) (Nodo 7 (Folha 3) (Folha 2))

-- ex1
multArvore:: Int -> Arvore -> Arvore
multArvore x (Folha n) = Folha(x*n)
multArvore x (Nodo n a1 a2) = Nodo (x*n) (multArvore x a1) (multArvore x a2)

-- ex2
contaFolhas :: Arvore -> Int
contaFolhas (Folha n) = 1
contaFolhas (Nodo n a1 a2) = (contaFolhas a1) + (contaFolhas a2)

-- ex3
contaNodos :: Arvore -> Int
contaNodos (Folha n) = 1
contaNodos (Nodo n a1 a2) = 1 + (contaNodos a1) + (contaNodos a2)