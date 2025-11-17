module Quad where

import Cp

-- (1) Datatype definition -----------------------------------------------------

data QTree a = Pixel a | Blocks ((QTree a, QTree a), (QTree a, QTree a))
  deriving (Show)

inQTree :: Either a ((QTree a, QTree a), (QTree a, QTree a)) -> QTree a
inQTree = either Pixel Blocks

outQTree :: QTree a -> Either a ((QTree a, QTree a), (QTree a, QTree a))
outQTree (Pixel x) = i1 x
outQTree (Blocks ((x, y), (z, w))) = i2 ((x, y), (z, w))

baseQTree g f = g -|- ((f >< f) >< (f >< f))

-- (2) Ana + cata + hylo -------------------------------------------------------

recQTree f = baseQTree id f

cataQTree g = g . (recQTree (cataQTree g)) . outQTree

-- TODO
-- anaQTree f = undefined

-- TODO
-- hyloQTree f = undefined

-- (3) Map ---------------------------------------------------------------------

instance Functor QTree where
  fmap f = cataQTree (inQTree . baseQTree f id)

-- (4) Examples ----------------------------------------------------------------

-- (4.0) Inversion (mirror) ----------------------------------------------------

mirrorQTree = cataQTree (either Pixel (Blocks . swap . (swap >< swap)))

-- (4.1) Count and depth -------------------------------------------------------

countQTree = cataQTree (either one (add . (add >< add)))

depthQTree = cataQTree (either one (succ . umax . (umax >< umax)))

-- (4.2) Rotate 90º ------------------------------------------------------------

f ((x, y), (z, w)) = ((w, x), (y, z))

rotate90 = cataQTree (either Pixel (Blocks . f))

-- (4.3) Serialization ---------------------------------------------------------

tips = cataQTree (either singl (conc . (conc >< conc)))

-- (5) Testing Examples --------------------------------------------------------

-- Example in worksheet 09
img :: QTree Bool
img = Blocks (
    (Pixel True,
     Blocks (
        (Pixel True, 
         Pixel True), 
        (Blocks (
            (Pixel False,
             Pixel True), 
            (Pixel True,
             Pixel True)),
         Pixel False))
    ), 
    (Blocks (
        (Pixel False, 
         Pixel True), 
        (Blocks (
            (Pixel True,
             Pixel True), 
            (Pixel False,
             Pixel True)),
         Pixel True)),
     Pixel False))

---------------------------- end of library ------------------------------------
