{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Lab2 where

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

(%) :: N -> N -> N
(%) = \m -> \n -> case m of {
    O -> O;
    S x -> case n of {
        O -> S(O);
        S y -> (m%y)* m
    }
}

par :: N -> Bool
par = \n -> case n of {
  O -> True;
  S x -> not (par x)
}

impar :: N -> Bool
impar = \n -> case n of {
  O -> False;
  S x -> not(impar x)
}

doble :: N -> N
doble = \n -> n * S(S(O))

triple :: N -> N
triple = \n -> case n of {
    O -> O;
    S x -> S(S(S(triple (x))))
}

fact :: N -> N
fact = \n -> case n of {
    O -> S(O);
    S x -> (fact x)*n
}

sumi :: N -> N
sumi = \n -> case n of {
    O -> O;
    S x -> (sumi x)+n
}

sumdobles :: N -> N
sumdobles = \n -> case n of {
    O -> O;
    S x -> (sumdobles x) + doble n
}

sumfacts :: N -> N 
sumfacts = \n -> case n of {
    O -> O;
    S x -> (sumfacts x) + fact n
}

sumfactss :: N -> N 
sumfactss = \n -> sumfi fact n

sumfi :: (N->N)-> N -> N
sumfi = \p -> \n -> case n of {
    O -> O;
    S x -> sumfi p x + p n
}

sumpares :: N -> N
sumpares = \n -> case n of {
    O -> O;
    S x -> case par n of {
        True -> sumpares x + n;
        False -> sumpares x;
    }
}

sumparess :: N -> N
sumparess = \n -> sumpi par n

sumimpares :: N -> N
sumimpares = \n -> case n of {
    O -> O;
    S x -> case impar n of {
        True -> sumimpares x + n;
        False -> sumimpares x;
    }
}

sumpi :: (N->Bool) -> N -> N
sumpi = \p -> \n -> case n of {
    O -> O;
    S x -> case p n of {
        True -> sumpi p x + n;
        False -> sumpi p x
    }
}