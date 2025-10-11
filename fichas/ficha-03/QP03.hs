
module QP03 where


import Cp
import Data.List


type Key = String
type Aut = String
type Pag = Int
type Bib = [(Key, [Aut])]
type Aux = [(Pag, [Key])]
type Ind = [(Aut, [Pag])]


testBib :: Bib
testBib =
  [ ("K_JM", ["Jones, C. B.", "Spivey, J.M."]),
    ("K_HS", ["Horowitz, E.", "Sahni, S."]),
    ("K_HU", ["Hudak, P."]),
    ("K_AM", ["Arbib, M. A.", "Manes, E. G."]),
    ("K_BJ", ["Bird, R.", "Jones, C. B."]),
    ("K_W", ["Wadler, P."])
  ]

testAux :: Aux
testAux =
  [ (11, ["K_AM", "K_HU"]),
    (3, ["K_HS", "K_JM", "K_W"]),
    (7, ["K_JM"]),
    (2, ["K_HS", "K_W"]),
    (10, ["K_AM"]),
    (15, ["K_HS"]),
    (16, ["K_HS"]),
    (19, ["K_HS"]),
    (28, ["K_BJ"]),
    (29, ["K_HU"]),
    (12, ["K_HU"])
  ]


mkInd :: (Bib, Aux) -> Ind
mkInd = map (id >< sort) . uncurry applyMapping . swap . (invertRelation >< invertRelation)

{- | Swaps the key-value relationship in a list of pairs of lists.

Each pair @(a, [b])@ is expanded so that each @b@ becomes a key,
associated with all @a@s that pointed to it in the original list.

== Example:

>>> invertRelation [("x", [1,2]), ("y", [2,3])]
[(1,["x"]),(2,["x","y"]),(3,["y"])]
-}
invertRelation :: (Ord a, Ord b) => [(a, [b])] -> [(b, [a])]
invertRelation = groupPairs . sortOn p1 . map swap . expandPairs

{- | Expands each pair @(a, [b])@ into multiple pairs @(a, b)@,
     one for each element of the list.

== Example:

>>> expandPairs [("x", [1,2]), ("y", [3])]
[("x",1),("x",2),("y",3)]
-}
expandPairs :: [(a, [b])] -> [(a, b)]
expandPairs = concat . map (\(x, l) -> map (x,) l)

{- | Compresses a list of pairs into a list of grouped pairs.

Pairs with the same first element are grouped together,
and their second components are collected into a list.

The order of the input list determines grouping behavior.

== Example:

>>> groupPairs [("x",1),("x",2),("y",3)]
[("x",[1,2]),("y",[3])]
-}
groupPairs :: (Eq a) => [(a, b)] -> [(a, [b])]
groupPairs = uncurry zip . split (nub . map p1) 
                                 (map (map p2) . groupBy (curry (uncurry (==) . (p1 >< p1))))

{- | Replaces values in a list of associations according to a mapping.

Given a dictionary @[(b, [c])]@ and a list @[(a, [b])]@,
every @b@ in the second list is replaced by its corresponding
list of @c@s from the first list (concatenated if multiple match).

== Example:

>>> applyMapping [("x", [1,2]), ("y", [3])] [("a", ["x"]), ("b", ["y","x"])]
[("a",[1,2]),("b",[3,1,2])]
-}
applyMapping :: (Eq b) => [(b, [c])] -> [(a, [b])] -> [(a, [c])]
applyMapping a = map (applyMappingOne a)

{- | Helper for 'applyMapping'. Replaces all elements of the second component
     of a pair according to a lookup table.

If an element is not found in the dictionary, it contributes nothing.

== Example:

>>> applyMappingOne [("x",[1]),("y",[2,3])] ("a", ["y","z","x"])
("a",[2,3,1])
-}
applyMappingOne :: (Eq b) => [(b, [c])] -> (a, [b]) -> (a, [c])
applyMappingOne d = id >< (concat . map (\ x -> case lookup x d of
                                                  Just xs -> xs
                                                  _ -> []))

