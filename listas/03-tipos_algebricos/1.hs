data Arvore = Folha Int | Nodo Int Arvore Arvore
    deriving(Eq, Show)

arv1 = Nodo 1 (Nodo 2 (Folha 3) (Folha 4)) (Folha 5)

multArvore :: Int -> Arvore -> Arvore
multArvore 1 a = a -- isso aqui logicamente pouparia esforços computacionais ne
multArvore n (Folha m) = (Folha (n * m)) -- como o retorno é uma folha aqui, precisa dos parenteses
multArvore n (Nodo m a1 a2) = Nodo (m * n) (multArvore n a1) (multArvore n a2) -- mesma ideia, como é uma arvore, precisa dos parenteses

