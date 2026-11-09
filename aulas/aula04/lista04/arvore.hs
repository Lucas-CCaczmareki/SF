data Arvore = Folha Int | Nodo Int Arvore Arvore
    deriving(Eq, Show)

arv1 :: Arvore
arv1 =
    Nodo 10 (Nodo 14 (Nodo 1 (Folha 4) (Folha 2)) (Folha 6)) (Folha 9)

arv2 :: Arvore
arv2 = Nodo 7 (Nodo 2 (Folha 4) (Folha 3)) (Nodo 7 (Folha 3) (Folha 2))

somaArvore :: Arvore -> Int
somaArvore (Folha n) = n
somaArvore (Nodo n a1 a2) = n + somaArvore a1 + somaArvore a2

multDoisArvore :: Arvore -> Arvore
multDoisArvore (Folha n) = Folha (2*n)
multDoisArvore (Nodo n a1 a2) = Nodo (2*n) (multDoisArvore a1) (multDoisArvore a2)