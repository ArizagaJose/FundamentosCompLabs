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