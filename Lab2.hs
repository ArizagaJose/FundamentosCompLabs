{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

data N where { O :: N ; S :: N -> N } deriving Show

uno :: N
uno = S O

dos :: N
dos = S uno

tres :: N
tres = S dos

cuatro :: N
cuatro = S tres

cinco :: N
cinco = S cuatro

predecesor :: N -> N
predecesor = \n -> case n of {O -> O; S x -> x}

instance Eq N where
    (==) :: N -> N -> Bool
    (==) = \n -> \m -> case n of {
        O -> case m of {
            O -> True;
            S y -> False;
        };
        S x -> case m of {
            O -> False;
            S y -> x==y;
        }
    }

instance Ord N where
    (<=) :: N -> N -> Bool
    (<=) = \m -> \n -> case m of {
        O -> True;
        S x -> case n of {
            O -> False;
            S y -> x <= y;
        }
    }

minimo :: N -> N -> N
minimo = \m -> \n -> case m>=n of {
    True -> n;
    False -> m;
}

maximo :: N -> N -> N
maximo = \m -> \n -> case m>=n of {
    True -> m;
    False -> n;
}

min3 :: N -> N -> N -> N
min3 = \m -> \n -> \r -> minimo (minimo m n) r

instance Num N where
    (+) :: N -> N -> N
    (+) =  \m -> \n -> case m of {
        O -> n;
        S x -> S (x + n);
    }
    (*) :: N -> N -> N
    (*) = \m -> \n -> case m of {
        O -> O;
        S x -> n + (x * n)
    }
    (-) :: N -> N -> N
    (-) = \m -> \n -> case m of {
        O -> O;
        S x -> case n of {
            O -> m;
            S y -> x - y;
        }
    }
