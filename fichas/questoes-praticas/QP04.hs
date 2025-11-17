module QP04 where

import Cp
import Data.Either

glue :: ([(k, v)], [(h, v)]) -> [(Either k h, v)]
glue = uncurry (++) . (map (i1 >< id) >< map (i2 >< id))

frLeft (Left x) = x

frRight (Right x) = x

unglue :: [(Either k h, v)] -> ([(k, v)], [(h, v)])
unglue =
  split
    (map (frLeft >< id) . filter (isLeft . p1))
    (map (frRight >< id) . filter (isRight . p1))
