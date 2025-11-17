module QP06 where

import Cp
import Data.List (nub)

func :: (Eq a) => b -> [(a, b)] -> (a -> b)
func b = (maybe b id .) . flip lookup

a :: [(Int, String)]
a = [(140999000, "Manuel"), (200100300, "Mary"), (000111222, "Teresa")]

b :: [(Int, String)]
b = [(140999000, "PT"), (200100300, "UK")]

c :: [(Int, String)]
c = [(140999000, "Braga"), (200100300, "Porto"), (151999000, "Lisbon")]

k :: [Int]
k = [140999000, 200100300, 000111222, 151999000]

as = func "(Unknown)" a

bs = func "?" b

cs = func "-" c

dataAgg = map (split (split id as) (split bs cs))

expr = map ((id >< flatr) . assocr) . dataAgg

f = nub $ map p1 c ++ map p1 b ++ map p1 a

-- printing functions (cortesy of ChatGPT)

prettyRow :: (Show a) => (a, (String, String, String)) -> String
prettyRow (i, (name, country, residence)) =
  pad 12 (show i)
    ++ pad 12 name
    ++ pad 10 country
    ++ residence
  where
    pad n s = s ++ replicate (max 0 (n - length s)) ' '

printTable :: IO ()
printTable = do
  putStrLn $ pad 12 "Id" ++ pad 12 "Name" ++ pad 10 "Country" ++ "Residence"
  putStrLn $ replicate 50 '-'
  mapM_ (putStrLn . prettyRow) (expr f)
  where
    pad n s = s ++ replicate (max 0 (n - length s)) ' '

main :: IO ()
main = printTable
