data Nat = Zero | Suc Nat
    deriving(Eq, Show)

soma :: Nat -> Nat -> Nat
soma Zero n = n                     -- n1 = Zero e n com todos os sucs. retorna
soma (Suc n1) n2 = soma n1 (Suc n2) -- tirar um suc do n1 e colocar no n2 até o n1 ser zero


mult :: Nat -> Nat -> Nat
mult Zero n = Zero
-- tem que se ligar que dá pra dividr as coisas em funções diferentes
-- eu fiquei meia hora tentando pensar num jeito de somar n vezes o segundo númro numa linha só
-- quando dava pra só fazer uma função que somava o n2 e chamava mult recursiva n vezes
mult (Suc n1) n2 = soma n2 (mult n1 n2)