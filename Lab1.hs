{-#LANGUAGE GADTs, EmptyDataDecls, EmptyCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Bool where

import Prelude (Show)

data Bool where {
    True :: Bool;
    False :: Bool
} deriving Show

not :: Bool -> Bool
not = \b -> case b of {
    True -> False;
    False -> True
}

(||) :: Bool -> Bool -> Bool
(||) = \b1 -> \b2 -> case b1 of {
    True -> True;
    False -> b2
}

(&&) :: Bool -> Bool -> Bool
(&&) = \b1 -> \b2 -> case b1 of {
    True -> b2;
    False -> False
}

xor :: Bool -> Bool -> Bool
xor = \b1 -> \b2 -> case b1 of {
    True -> not b2;
    False -> b1 || b2
}
-- b1   b2  xor
-- 1    1   0
-- 0    1   1
-- 1    0   1
-- 0    0   0

(>>) :: Bool -> Bool -> Bool
(>>) = \b1 -> \b2 -> case b1 of {
    True -> True;
    False -> not b2;
}
-- b1   b2  >>
-- 1    1   1
-- 0    1   0
-- 1    0   1
-- 0    0   1

--Igualdad con case
(===) :: Bool -> Bool -> Bool
(===) = \b1 -> \b2 -> case b1 of {
    True -> b1 && b2;
    False -> not b2;
}

--Igualdad sin case
(==) :: Bool -> Bool -> Bool
(==) = \b1 -> \b2 -> not(xor b1 b2)
-- b1   b2  ==
-- 1    1   1
-- 0    1   0
-- 1    0   0
-- 0    0   1

(/=) :: Bool -> Bool -> Bool
(/=) = \b1 -> \b2 -> not(b1 == b2)
--es lo mismo que el xor, te das cuenta viendo la tabla de verdad o
--mismo sabiendo que estas haciendo not de == que a su vez es not de xor
--por ende es xor

(<) :: Bool -> Bool -> Bool
(<) = \b1 -> \b2 -> not b1 >> b2
-- b1   b2  <
-- 1    1   0
-- 0    1   1
-- 1    0   0
-- 0    0   0

--unanimidad con case
unanimidad :: Bool -> Bool -> Bool -> Bool
unanimidad = \b1 -> \b2 -> \b3 -> case b1 of {
    True -> b2 && b3;
    False -> not b2 && not b3
}
-- b1   b2  b3  unanimidad
-- 1    1   1   1
-- 0    1   1   
-- 1    0   1
-- 0    0   1
-- 1    1   0
-- 0    1   0
-- 1    0   0
-- 0    0   0   1

--unanimidad sin case
unanimidadd :: Bool -> Bool -> Bool -> Bool
unanimidadd = \b1 -> \b2 -> \b3 -> (b1 == b2) == (b2 == b3)

--mayoria con case
mayoria :: Bool -> Bool -> Bool -> Bool
mayoria = \b1 -> \b2 -> \b3 -> case b1 of {
    True -> b2 || b3;
    False -> b2 && b3
}

--mayoria sin case
mayoriaa :: Bool -> Bool -> Bool -> Bool
mayoriaa = \b1 -> \b2 -> \b3 -> (b1 || b2) && (b2 ||b3);

-- b1   b2  b3  mayoria
-- 1    1   1   1
-- 0    1   1   1
-- 1    0   1   1
-- 0    0   1   0
-- 1    1   0   1
-- 0    1   0   0
-- 1    0   0   0
-- 0    0   0   0

--impar con case
impar :: Bool -> Bool -> Bool -> Bool
impar = \b1 -> \b2 -> \b3 -> case b1 of {
    True -> b2 == b3;
    False -> xor b2 b3
}

--impar sin case
imparr :: Bool -> Bool -> Bool -> Bool
imparr = \b1 -> \b2 -> \b3 -> (b1 && (b2 && b3)) || (xor b1 (xor b2 b3)); 

-- b1   b2  b3  impar
-- 1    1   1   1
-- 0    1   1   0
-- 1    0   1   0
-- 0    0   1   1
-- 1    1   0   0
-- 0    1   0   1
-- 1    0   0   1
-- 0    0   0   0
