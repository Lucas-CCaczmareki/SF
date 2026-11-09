data Arvore = Folha Int | Nodo Int Arvore Arvore
    deriving(Eq, Show)


arv :: Arvore
arv = Nodo 7 (Nodo 2 (Folha 4) (Folha 3)) (Nodo 7 (Folha 3) (Folha 2))

multArvore:: Int -> Arvore -> Arvore
multArvore x (Folha n) = Folha(x*n)
multArvore x (Nodo n a1 a2) = Nodo (x*n) (multArvore x a1) (multArvore x a2)