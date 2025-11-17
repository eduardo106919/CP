module QP05 where

import Cp

nub' :: (Eq a) => [a] -> [a]
nub' = either nil cons . f

f :: (Eq a) => [a] -> Either () (a, [a])
f [] = i1 ()
f (h : t) = i2 (h, filter (/= h) (nub' t))
