module B_Tree where

import Cp

-- (1) Datatype definition -----------------------------------------------------

data B_Tree a = Nil | Block {leftmost :: B_Tree a, block :: [(a, B_Tree a)]}
  deriving (Show)

inB_Tree :: Either () (B_Tree a, [(a, B_Tree a)]) -> B_Tree a
inB_Tree = either (const Nil) (uncurry Block)

outB_Tree :: B_Tree a -> Either () (B_Tree a, [(a, B_Tree a)])
outB_Tree Nil = i1 ()
outB_Tree (Block l b) = i2 (l, b)

baseB_Tree g f = id -|- (f >< (map (g >< f)))

-- (2) Ana + cata + hylo -------------------------------------------------------

recB_Tree f = baseB_Tree id f

cataB_Tree g = g . (recB_Tree (cataB_Tree g)) . outB_Tree

anaB_Tree g = inB_Tree . (recB_Tree (anaB_Tree g)) . g

hyloB_Tree f g = cataB_Tree f . anaB_Tree g

-- (3) Map ---------------------------------------------------------------------

instance Functor B_Tree where
  fmap f = cataB_Tree (inB_Tree . baseB_Tree f id)

-- (4) Examples ----------------------------------------------------------------

-- (4.1) Count and depth -------------------------------------------------------

countB_Tree = cataB_Tree (either zero (add . (id >< sum . map (succ . p2))))

depthB_Tree = cataB_Tree (either zero (succ . umax . (id >< maximum . map p2)))

-- (4.2) Serialization ---------------------------------------------------------

-- in-order traversal
inordtB_Tree = cataB_Tree (either nil (conc . (id >< (concat . map cons))))

-- pre-order traversal
preordtB_Tree = cataB_Tree (either nil aux)
  where
    aux (l, []) = l
    aux (l, (h,a):t) = (h : l) ++ a ++ (concat (map cons t))


-- post-order traversal
postordtB_Tree = cataB_Tree (either nil (conc . (id >< (concat . map aux)))) where aux (x, l) = l ++ [x]

-- (x) B_Tree examples ---------------------------------------------------------

t :: B_Tree Integer
t =
  Block
    { leftmost =
        Block
          { leftmost = Nil,
            block = [(1, Nil), (2, Nil), (5, Nil), (6, Nil)]
          },
      block =
        [ ( 7,
            Block
              { leftmost = Nil,
                block = [(9, Nil), (12, Nil)]
              }
          ),
          ( 16,
            Block
              { leftmost = Nil,
                block = [(18, Nil), (21, Nil)]
              }
          )
        ]
    }
