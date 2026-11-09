data N = Zero | Suc N
    deriving(Eq, Show)

sub :: N -> N -> N
sub Zero n = Zero
sub n Zero = n
sub (Suc n1) (Suc n2) = sub n1 n2
