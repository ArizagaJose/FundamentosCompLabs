{-#LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}


module Lab3 where

import Lab2
import Prelude hiding (null,length,sum,map,zip,zipWith,filter,and,or,any,all,(++),reverse,elem,concat,head,tail,last,init,(!!),fst,snd,take,drop,takeWhile,dropWhile,split)

null :: [a] -> Bool
null = \l -> case l of {
    [] -> True;
    x:xs -> False;
}

length :: [a] -> N
length = \l -> case l of {
    [] -> O;
    x:xs -> S(length xs);
}

duplicate :: [a] -> [a]
duplicate = \l -> case l of {
    [] -> [];
    x:xs -> x:x:duplicate xs;
}

sum :: [N] -> N
sum = \l -> case l of {
    [] -> O;
    x:xs -> x+sum xs;
}

prod :: [N] -> N
prod = \l -> case l of {
    [] -> S(O);
    x:xs -> (prod xs * x);
}

map :: (a->b) -> [a] -> [b] 
map = \func -> \l -> case l of {
    [] -> [];
    x:xs -> func x:map func xs;
}

zipWith :: (a->b->c) -> [a] -> [b] -> [c]
zipWith = \func -> \l1 -> \l2 -> case l1 of {
    [] -> [];
    x:xs -> case l2 of {
        [] -> [];
        y:ys -> func x y:zipWith func xs ys;
    }
}

filter :: (a -> Bool) -> [a] -> [a]
filter = \func -> \l -> case l of {
    [] -> [];
    x:xs -> case func x of {
        True -> x:filter func xs;
        False -> filter func xs;
    }
}

and :: [Bool] -> Bool
and = \l -> case l of {
    [] -> True;
    x:xs -> x && and xs;
}

or :: [Bool] -> Bool
or = \l -> case l of {
    [] -> False;
    x:xs -> x || or xs;
}

count :: (a->Bool) ->[a]-> N
count = \func -> \l -> case l of {
    [] -> O;
    x:xs -> case func x of {
        True -> S(count func xs);
        False -> count func xs;
    }
}

any :: (a->Bool) ->[a]->Bool
any = \func -> \l -> case l of {
    [] -> False;
    x:xs -> case func x of {
        True -> True;
        False -> any func xs;
    }
}

all :: (a->Bool) ->[a]->Bool
all = \func -> \l -> case l of {
    [] -> True;
    x:xs -> case func x of {
        True -> all func xs;
        False -> False;
    }
}

countt :: (a->Bool) ->[a]-> N
countt = \func -> \l -> length (filter func l)

anyy :: (a->Bool) ->[a]->Bool
anyy = \func -> \l -> or (map func l)

alll :: (a->Bool) ->[a]->Bool
alll = \func -> \l -> and (map func l)

(++) :: [a]->[a]->[a]
(++) = \l1 -> \l2 -> case l1 of {
    [] -> l2;
    x:xs -> x:(xs ++ l2)
}

reverse :: [a]->[a]
reverse = \l -> case l of {
    [] -> [];
    x:xs -> reverse xs ++ [x];
}

elem :: Eq a => a -> [a]-> Bool
elem = \n -> \l -> case l of {
    [] -> False;
    x:xs -> case n==x of {
        True -> True;
        False -> elem n xs;
    }
}

concat :: [[a]] -> [a]
concat = \l1 -> case l1 of {
    []->[];
    x:xs -> x ++ concat xs
}

lensum :: [[a]] -> N
lensum = \l -> case l of {
    [] -> O;
    x:xs -> length x + lensum xs
}

fst :: (a,b) -> a
fst = \p -> case p of {
    (primero, segundo) -> primero;
}

snd :: (a,b) -> b
snd = \p -> case p of {
    (primero, segundo) -> segundo;
}

{- pares ordenados -}


pre :: Bool -> String -> a -> a
pre = \b -> \s -> \x -> case b of {
    True -> x;
    False -> error s;
}

head :: [a] -> a
head = \l -> case l of {
    []
}
