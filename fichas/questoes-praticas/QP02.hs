module QP02 where

import Cp

acronym :: String -> String
acronym = map head . words

short :: String -> String
short = uncurry (++) . (id >< (' ' :)) . split head last . words
