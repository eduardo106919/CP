\documentclass[11pt, a4paper, fleqn]{article}
\usepackage{cptemplate}

%================= lhs2tex =====================================================%
%include polycode.fmt
%%format (bin (n) (k)) = "\Big(\vcenter{\xymatrix@R=1pt{" n "\\" k "}}\Big)"
%format -|- = "+"
%format . = "\comp "
%format .* = "\star " 
%format .&&&. = "\wedge"
%format .<=. = "\leq"
%format .<==>. = "\Leftrightarrow"
%format .=?=. = "\mathbin{\stackrel{\mathrm{?}}{=}}"
%format .==. = "\equiv"
%format .==>. = "\Rightarrow"
%format (ana (g)) = "\ana{" g "}"
%format (anaNat (g)) = "\ana{" g "}"
%format (ana' (f) (g)) = "\lanabracket\;\!" f "\:" g "\:\!\ranabracket"
%format (anaForest (f) (g)) = "\lanabracket\;\!" f "\:" g "\:\!\ranabracket_{\textit{\tiny F}}"
%format (anaList (g)) = "\anaList{" g "}"
%format (anaLTree (g)) = "\lanabracket\," g "\,\ranabracket"
%format (anaStream (g)) = "\lanabracket\," g "\,\ranabracket"
%format (anaRose (g)) = "\lanabracket\," g "\,\ranabracket_\textit{\tiny R}"
%format (anaTree (f) (g)) = "\lanabracket\;\!" f "\:" g "\:\!\ranabracket_{\textit{\tiny T}}"
%format (cata (f)) = "\llparenthesis\, " f "\,\rrparenthesis"
%format (cata' (f) (g)) = "\llparenthesis\, " f "\:" g "\,\rrparenthesis"
%format (cataBTree (x)) = "\llparenthesis\, " x "\,\rrparenthesis"
%format (cataLTree (x)) = "\llparenthesis\, " x "\,\rrparenthesis"
%format (cataForest (f) (g)) = "\llparenthesis\, " f "\:" g "\,\rrparenthesis_{\textit{\tiny F}}"
%format (cataFTree (x)) = "\llparenthesis\, " x "\,\rrparenthesis"
%format (cataList (g)) = "\llparenthesis\, " g "\,\rrparenthesis"
%format (cataNat (g)) = "\cataNat{" g "}"
%format (cataB_Tree (g)) = "\cataNat{" g "}"
%format (cataRose (x)) = "\llparenthesis\, " x "\,\rrparenthesis_\textit{\tiny R}"
%format (cataTree (f) (g)) = "\llparenthesis\, " f "\:" g "\,\rrparenthesis_{\textit{\tiny T}}"
%format (const (f)) = "\underline{" f "}"
%format (Cp.cond (p) (f) (g)) = "\mcond{" p "}{" f "}{" g "}"
%format (curry (f)) = "\overline{" f "}"
%format (div (x)(y)) = x "\div " y
%format (either (a) (b)) = "\alt{" a "}{" b "}"
%format (for (f) (i)) = "\for{" f "}\ {" i "}"
%format (frac (a) (b)) = "\frac{" a "}{" b "}"
%format (frac (n)(m)) = "\frac{" n "}{" m "}"
%format (frac3 (n)(m)(o)) = "\frac{" n "\\" m "}{" o "}"
%format (hylo (g) (h)) = "\llbracket\, " g ",\," h "\,\rrbracket"
%format (hylo' (ft) (ff) (gt) (gf)) = "\llbracket\, " ft "\:" ff ",\," gt "\:" gf "\,\rrbracket"
%format (hyloForest (ft) (ff) (gt) (gf)) = "\llbracket\, " ft "\:" ff ",\," gt "\:" gf "\,\rrbracket_{\textit{\tiny F}}"
%format (hyloRose (g) (h)) = "\llbracket\, " g ",\," h "\,\rrbracket_\textit{\tiny R}"
%format (hyloTree (ft) (ff) (gt) (gf)) = "\llbracket\, " ft "\:" ff ",\," gt "\:" gf "\,\rrbracket_{\textit{\tiny T}}"
%format (kcomp (f)(g)) = f "\kcomp " g
%format (lcbr (x)(y)) = "\begin{lcbr}" x "\\" y "\end{lcbr}"
%format (lcbr3 (x)(y)(z)) = "\begin{lcbr}" x "\\" y "\\" z "\end{lcbr}"
%format (lcbr4 (x)(y)(z)(w)) = "\begin{lcbr}" x "\\" y "\\" z "\\" w "\end{lcbr}"
%format (plus (f)(g)) = "{" f "}\plus{" g "}"
%format (Prod (a) (b)) = a >< b
%format (Seq (a)) = "{" a "}^{*}"
%format (split (x) (y)) = "\conj{" x "}{" y "}"
%format (square (x)) = x "^2"
%format (uncurry f) = "\uncurry{" f "}"
%format (underbrace (t) (p)) = "\underbrace{" t "}_{" p "}"
%format % = "\mathbin{/}"
%format `minusNat`= "\mathbin{-}"
%format `ominus` = "\mathbin{\ominus}"
%format ++ = "\mathbin{+\!\!+}"
%format <-> = "{\,\leftrightarrow\,}"
%format <|> = "{\,\updownarrow\,}"
%format <$> = "\mathbin{\mathopen{\langle}\$\mathclose{\rangle}}"
%format ==> = "\Longrightarrow "
%format ==> = "\Rightarrow"
%format >< = "\times"
%format >|<  = "\bowtie "
%format |-> = "\mapsto"
%format B_tree = "\mathsf{B}\mbox{-}\mathsf{tree} "
%format cdots = "\cdots "
%format conc = "\mathsf{conc}"
%format delta = "\Delta "
%format Dist = "\fun{Dist}"
%format Either a b = a "+" b
%format fF = "\fun F "
%format fmap = "\mathsf{fmap}"
%format fromRational = " from_\Q "
%format fst = "\p1"
%format FTree = "{\FTree}"
%format i1 = " i_1"
%format i2 = " i_2"
%format inForest = "\mathsf{in}_{Forest}"
%format inFTree = "\mathsf{in}"
%format inLTree = "\mathsf{in}"
%format inNat = "\mathsf{in}"
%format inT = "\mathsf{in}"
%format inList = "\mathsf{in}"
%format Integer  = "\mathbb{Z}"
%format inTree = "\mathsf{in}_{Tree}"
%format inBTree = "\mathsf{in}"
%format inExpr = "\mathsf{in}"
%format IO = "\fun{IO}"
%format l2 = "l_2 "
%format Left = "i_1"
%format length = "\length "
%format LTree = "{\LTree}"
%format map = "\map "
%format matrix = "matrix"
%format muB = "\mu "
%format NA   = "\textsc{na}"
%format Nat0 = "\N_0"
%format NB   = "\textbf{NB}"
%format Null = "1"
%format outForest = "\mathsf{out}_{Forest}"
%format outFTree = "\mathsf{out}"
%format outLTree = "\mathsf{out}"
%format outStream = "\mathsf{out}"
%format outT = "\mathsf{out}"
%format outTree = "\mathsf{out}_{Tree}"
%format outList = "\mathsf{out}"
%format outNat = "\mathsf{out}"
%format outBTree = "\mathsf{out}"
%format outExpr = "\mathsf{out}"
%format p1 = "\p1"
%format p2 = "\p2"
%format pi = "\pi "
%format Rational = "\Q "
%format Right = "i_2"
%format snd = "\p2"
%format succ = "\succ"
%format summation = "{\sum}"
%format TLTree = "\mathsf{TLTree}"
%format toRational = " to_\Q "
%format beta = "\beta"
%format alfa = "\alpha"
%format nabla = "\nabla"
%format theta = "\theta"
%format (iso(f)) = f "^{\circ}"
%format f1 = " f_1"
%format f2 = " f_2"
%==============================================================================%


\title{Cálculo de Programas \\ Resolução - Ficha 10}
\author{Eduardo Freitas Fernandes}
\date{2026}

%----------------- Início do documento -------------------------------------------%
\begin{document}

\sffamily
\renewcommand{\baselinestretch}{1.25} 
\pagestyle{pagestyle}

\newgeometry{left=25mm,right=20mm,top=25mm,bottom=25mm}
\setlength{\parindent}{1em}

%if False
\begin{code}
{-# OPTIONS_GHC -XNPlusKPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, FlexibleInstances #-}
module Main where
import Cp
import List
import Data.List (sort, nub)

main = undefined
\end{code}
%endif


\maketitle


\noindent {\large \textbf{Exercício 1}}
\begin{eqnarray*}
\xymatrix@@C=3cm@@R=1cm{
    |Seq(A)|
           \ar[d]_-{| anaList g |}
           \ar@@/^1pc/[r]^-{| outList |}
&
    |A + square((LTree A))|
           \ar[d]^{| id + id >< anaList g |}
           \ar@@/^1pc/[l]^-{| inList |}
\\
     |A|
          \ar@@/_1pc/[r]_-{| g |}
&
     |A + square(A)|
}
\end{eqnarray*}

\noindent O anamorfismo de |g| inverte uma lista, ou seja, é a função |reverse|.


\noindent {\large \textbf{Exercício 2}}
\begin{eqnarray*}
\xymatrix@@C=3cm@@R=1cm{
    |Nat0|
           \ar[d]_-{| uncurry theta |}
           \ar@@/^1pc/[r]^-{| outNat |}
&
    |1 + Nat0|
           \ar[d]^{| id + anaNat g |}
           \ar@@/^1pc/[l]^-{| inNat |}
\\
     |Nat0 >< Nat0|
          \ar@@/_1pc/[r]_-{| g |}
&
     |1 + Nat0 >< Nat0|
}
\end{eqnarray*}
\begin{code}
g :: (Int, Int) -> Either () (Int, Int)
g = Cp.cond (uncurry (<=)) (i1 . (!)) (i2 . (id >< succ))
\end{code}


\noindent {\large \textbf{Exercício 3}}
\begin{eqnarray*}
\start
|
     length . concat = sum . map length
|
\just\equiv{ def. |concat|, def. |sum| }
|
     length . cataList (either nil conc) = cataList (either (const 0) add) . map length
|
\just\equiv{ absorção-cata }
|
     length . cataList (either nil conc) = cataList (either (const 0) add . (id + length >< id))
|
\just\impliedby{ absorção-|+|, fusão-cata }
|
     length . either nil conc = either (const 0) (add . (length >< id)) . (id + id >< length)
|
\just\equiv{ fusão-|+|, absorção-|+|, eq-|+| }
|
lcbr(
     length . nil = const 0
)(
     length . conc = add . (length >< id) . (id >< length)
)
|
\just\equiv{ functor-|><| }
|
     length . conc = add . (length >< length)
|
\just\equiv{ ?????? }
|
     true
|
\qed
\end{eqnarray*}


\noindent {\large \textbf{Exercício 4}}
\begin{eqnarray*}
\start
|
     length = sum . (map (const 1))
|
\just\equiv{ def. |sum| }
|
     length = cataList (either (const 0) add) . map (const 1)
|
\just\equiv{ absorção-cata }
|
     length = cataList (either (const 0) add . B (const 1, id))
|
\just\equiv{ def. bi-functor de listas }
|
     length = cataList (either (const 0) add . (id + (const 1) >< id))
|
\just\equiv{ absorção-|+| }
|
     length = cataList (either (const 0) (add . (const 1 >< id)))
|
\just\equiv{ |add . (const 1 >< id) = succ . p2| }
|
     length = cataList (either (const 0) (succ . p2))
|
\end{eqnarray*}

\noindent Podemos verificar que | add . (const 1 >< id) | é equivalente a | succ . p2 |, pois esta recebe um par de valores, destroi o primeiro (transformando no valor 1) e mantém o segundo, de seguida soma o segundo a 1, obtendo assim o valor equivalente ao seu sucessor.
\begin{eqnarray*}
\start
|
     length = length . (map f)
|
\just\equiv{ def. |length| }
|
     length = cataList (either (const 0) (succ . p2)) . (map f)
|
\just\equiv{ absorção-cata }
|
     length = cataList (either (const 0) (succ . p2) . B (f, id))
|
\just\equiv{ def. bi-functor de listas }
|
     length = cataList (either (const 0) (succ . p2) . (id + f >< id))
|
\just\equiv{ absorção-|+|, natural-|id| }
|
     lengt = cataList (either (const 0) (succ . p2 . (f >< id)))
|
\just\equiv{ natural-|p2| }
|
     length = cataList (either (const 0) (succ . id . p2))
|
\just\equiv{ natural-|id| }
|
     length = cataList (either (const 0) (succ . p2))
|
\end{eqnarray*}


\noindent {\large \textbf{Exercício 5}}
\begin{eqnarray*}
\start
|
     depth . LTree f = depth
|
\just\equiv{ def. |depth| }
|
     cataLTree (either (const 1) (succ . umax)) . T f = depth
|
\just\equiv{ absorção-cata }
|
     cataLTree (either (const 1) (succ . umax) . B (f, id)) = depth
|
\just\equiv{ def. bi-functor de LTree }
|
     cataLTree (either (const 1) (succ . umax) . (f + (id >< id))) = depth
|
\just\equiv{ functor-id-|><|, absorção-|+| }
|
     cataLTree (either (const 1 . f) (succ . umax . (id >< id))) = depth
|
\just\equiv{ fusão-const, natural-|id| }
|
     cataLTree (either (const 1) (succ . umax)) = depth
|
\end{eqnarray*}




\noindent {\large \textbf{Exercício 6}}
\begin{spec}
bubble (x:y:xs)
     | x > y = y : bubble (x:xs)
     | otherwise = x : bubble (y:xs)
bubble x = x
\end{spec}

\noindent O primeiro passo será substituir o nome da função por |divide| e de seguida remover as chamadas recursivas:
\begin{spec}
divide (x:y:xs)
     | x > y = y ... (x:xs)
     | otherwise = x ... (y:xs)
divide x = x
\end{spec}

\noindent De seguida emparelhamos o resultado e por fim injetamos o resultado num co-produto, dado que existem dois tipos de resultado:
\begin{code}
divide (x:y:xs)
     | x > y = i2 (y, (x:xs))
     | otherwise = i2 (x, (y:xs))
divide x = i1 x
\end{code}

\noindent Podemos então inferir o tipo da função |divide|:
\begin{eqnarray*}
\start
|
     divide : Seq(A) -> Seq(A*) + A >< Seq(A)
|
\end{eqnarray*}
\noindent Verificamos que o bi-functor necessário para formar este hilomorfismo será o das \textbf{Listas com Sentinela}:
\begin{code}
data SList a b = Stl b | Cons a (SList a b)
\end{code}
\noindent Temos então:
\begin{eqnarray*}
\start
|
     B (Z, X, Y) = Z + X + Y
|
\end{eqnarray*}
\noindent Pelo tipo de |divide| podemos inferir o tipo de |conquer|:
\begin{code}
conquer :: Either [a] (a, [a]) -> [a]
conquer = either id cons
\end{code}

\newpage

\noindent {\large \textbf{Exercício 7}}
\begin{code}
data Point a = Point {x :: a, y :: a, z :: a} deriving (Eq, Show)

outPoint = split (split x y) z

-- inPoint = uncurry (uncurry Point)
inPoint ((a,b),c) = Point a b c
\end{code}

\noindent {\large \textbf{Exercício 8}}
\begin{code}
module B_Tree where

import Cp

-- (1) Datatype definition

data B_Tree a = Nil | Block {leftmost :: B_Tree a, block :: [(a, B_Tree a)]}
     deriving (Show)

inB_Tree :: Either () (B_Tree a, [(a, B_Tree a)]) -> B_Tree a
inB_Tree = either (const Nil) (uncurry Block)

outB_Tree :: B_Tree a -> Either () (B_Tree a, [(a, B_Tree a)])
outB_Tree Nil = i1 ()
outB_Tree (Block l b) = i2 (l, b)

baseB_Tree g f = id -|- (f >< (map (g >< f)))

-- (2) Ana + cata + hylo

recB_Tree f = baseB_Tree id f

cataB_Tree g = g . (recB_Tree (cataB_Tree g)) . outB_Tree

anaB_Tree g = inB_Tree . (recB_Tree (anaB_Tree g)) . g

hyloB_Tree f g = cataB_Tree f . anaB_Tree g

-- (3) Map

instance Functor B_Tree where
fmap f = cataB_Tree (inB_Tree . baseB_Tree f id)

-- (4) Examples

-- (4.1) Count and depth

countB_Tree = cataB_Tree (either zero 
                                        (add . (id >< sum . map (succ . p2))))

depthB_Tree = cataB_Tree (either zero 
                                        (succ . umax . (id >< maximum . map p2)))

-- (4.2) Serialization 

-- in-order traversal
inordtB_Tree = cataB_Tree (either nil (conc . (id >< (concat . map cons))))

-- pre-order traversal
preordtB_Tree = cataB_Tree (either nil aux)
     where
          aux (l, []) = l
          aux (l, (h,a):t) = (h : l) ++ a ++ (concat (map cons t))

-- post-order traversal
postordtB_Tree = cataB_Tree (either nil (conc . (id >< (concat . map aux))))
     where
          aux (x, l) = l ++ [x]
\end{code}

\end{document}
%----------------- Fim do documento -------------------------------------------%
