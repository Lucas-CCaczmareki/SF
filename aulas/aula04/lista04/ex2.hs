data Arvore = Folha Int | Nodo Int Arvore Arvore
    deriving(Eq, Show)


arv :: Arvore
arv = Nodo 7 (Nodo 2 (Folha 4) (Folha 3)) (Nodo 7 (Folha 3) (Folha 2))

contaFolhas :: Arvore -> Int
contaFolhas (Folha n) = 1
contaFolhas (Nodo n a1 a2) = (contaFolhas a1) + (contaFolhas a2)