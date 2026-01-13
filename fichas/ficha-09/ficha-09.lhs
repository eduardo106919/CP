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
%format (cataForest (f) (g)) = "\llparenthesis\, " f "\:" g "\,\rrparenthesis_{\textit{\tiny F}}"
%format (cataFTree (x)) = "\llparenthesis\, " x "\,\rrparenthesis"
%format (cataList (g)) = "\llparenthesis\, " g "\,\rrparenthesis"
%format (cataNat (g)) = "\cataNat{" g "}"
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
%format (iso(f)) = f "^{\circ}"
%format f1 = " f_1"
%format f2 = " f_2"
%==============================================================================%


\title{Cálculo de Programas \\ Resolução - Ficha 09}
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
\end{code}
%endif


\maketitle


\noindent {\large \textbf{Exercício 1}}

\begin{minipage}{0.6\textwidth}
\begin{eqnarray*}
\xymatrix@@C=3cm@@R=1cm{
    |LTree A|
           \ar[d]_-{| maximum |}
           \ar@@/^1pc/[r]^-{| outLTree |}
&
    |A + square((LTree A))|
           \ar[d]^{| id + square(maximum) |}
           \ar@@/^1pc/[l]^-{| inLTree |}
\\
     |A|
&
     |A + square(A)|
           \ar@@/^1pc/[l]^-{| gmaximum |}
}
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.4\textwidth}
\begin{code}
gmaximum = either id umax
\end{code}
\end{minipage}




\begin{minipage}{0.6\textwidth}
\begin{eqnarray*}
\xymatrix@@C=3cm@@R=1cm{
    |BTree A|
           \ar[d]_-{| inorder |}
           \ar@@/^1pc/[r]^-{| outBTree |}
&
    |1 + A >< square((BTree A))|
           \ar[d]^{| id + id >< square(inorder) |}
           \ar@@/^1pc/[l]^-{| inBTree |}
\\
     |Seq(A)|
&
     |1 + A >< square((Seq(A)))|
           \ar@@/^1pc/[l]^-{| ginorder |}
}
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.4\textwidth}
\begin{code}
ginorder = either nil aux
     where aux (h, (l,r)) = l ++ [h] ++ r
\end{code}
\end{minipage}


\begin{minipage}{0.6\textwidth}
\begin{eqnarray*}
\xymatrix@@C=3cm@@R=1cm{
    |BTree A|
           \ar[d]_-{| mirror |}
           \ar@@/^1pc/[r]^-{| outBTree |}
&
    |1 + A >< square((BTree A))|
           \ar[d]^{| id + id >< square(mirror) |}
           \ar@@/^1pc/[l]^-{| inBTree |}
\\
     |BTree A|
&
     |1 + A >< square((BTree A))|
           \ar@@/^1pc/[l]^-{| gmirror |}
}
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.4\textwidth}
\begin{code}
gmirror = either (const Empty) (Node . (id >< swap))
\end{code}
\end{minipage}




\begin{minipage}{0.6\textwidth}
\begin{eqnarray*}
\xymatrix@@C=3cm@@R=1cm{
    |LTree A|
           \ar[d]_-{| rep a |}
           \ar@@/^1pc/[r]^-{| outLTree |}
&
    |A + square((LTree A))|
           \ar[d]^{| id + square((rep a)) |}
           \ar@@/^1pc/[l]^-{| inLTree |}
\\
     |LTree A|
&
     |A + square((LTree A))|
           \ar@@/^1pc/[l]^-{| grep |}
}
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.4\textwidth}
\begin{code}
grep a = either (const a) Fork
\end{code}
\end{minipage}


\begin{minipage}{0.6\textwidth}
\begin{eqnarray*}
\xymatrix@@C=3cm@@R=1cm{
    |FTree B A|
           \ar[d]_-{| convert |}
           \ar@@/^1pc/[r]^-{| outFTree |}
&
    |B + A >< square((FTree B A))|
           \ar[d]^{| id + id >< square(convert) |}
           \ar@@/^1pc/[l]^-{| inFTree |}
\\
     |BTree A|
&
     |B + A >< square((BTree A))|
           \ar@@/^1pc/[l]^-{| gconvert |}
}
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.4\textwidth}
\begin{code}
gconvert = either (const Empty) Node
\end{code}
\end{minipage}




\begin{minipage}{0.6\textwidth}
\begin{eqnarray*}
\xymatrix@@C=3cm@@R=1cm{
    |Expr V O|
           \ar[d]_-{| vars |}
           \ar@@/^1pc/[r]^-{| outExpr |}
&
    |V + O >< Seq((Expr V O))|
           \ar[d]^{| id + id >< map vars |}
           \ar@@/^1pc/[l]^-{| inExpr |}
\\
     |Seq(V)|
&
     |V + O >< Seq(Seq(V))|
           \ar@@/^1pc/[l]^-{| gvars |}
}
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.4\textwidth}
\begin{code}
gvars = either singl (concat . p2)
\end{code}
\end{minipage}




\noindent {\large \textbf{Exercício 2}}
\begin{eqnarray*}
\start
|
     tar = cataBTree (either (singl . nil) g)
|
\just\equiv{ universal-cata }
|
     tar. either (const Empty) Node = either (singl . nil) g . (id + id >< (tar >< tar))
|
\just\equiv{ fusão-|+|, absorção-|+| }
|
     either (tar . const Empty) (tar . Node) = either (singl . nil) (g . (id >< (tar >< tar)))
|
\just\equiv{ eq-|+| }
|
lcbr(
     tar . const Empty = singl . nil
)(
     tar . Node = g . (id >< (tar >< tar))
)
|
\just\equiv{ pointwise, def. comp }
|
lcbr(
     tar Empty = [[]]
)(
     tar (Node (x, (l,r))) = g (x, (tar l, tar r))
)
|
\just\equiv{ def. g }
|
lcbr(
     tar Empty = [[]]
)(
     tar (Node (x, (l,r))) = (map cons . lstr) (x, tar l ++ tar r)
)
|
\just\equiv{ def. comp, def. |lstr| }
|
lcbr(
     tar Empty = [[]]
)(
     tar (Node (x, (l,r))) = map cons [ (x,a) || a <- tar l ++ tar r ]
)
|
\just\equiv{ def. map cons }
|
lcbr(
     tar Empty = [[]]
)(
     tar (Node (x, (l,r))) = [ h:t || (h,t) <- [ (x,a) || a <- tar l ++ tar r ] ]
)
|
\end{eqnarray*}


\noindent {\large \textbf{Exercício 3}}
\begin{eqnarray*}
\start
|
     vars = cata (either singl (concat . p2))
|
\just\equiv{ universal-cata }
|
     vars . either Var Term = either singl (concat . p2) . (id + id >< map vars)
|
\just\equiv{ fusão-|+|, absorção-|+|, eq-|+| }
|
lcbr(
     vars . Var = singl
)(
     vars . Term = concat . p2 . (id >< map vars)
)
|
\just\equiv{ natural-|p2|, pointwise }
|
lcbr(
     vars (Var v) = [v]
)(
     vars (Term (o, l)) = concat (map vars l)
)
|
\end{eqnarray*}


\noindent {\large \textbf{Exercício 4}}
\begin{eqnarray*}
\start
|
     k = anaList ((id + split f id) . outNat )
|
\just\equiv{ universal-ana }
|
     outList . k = (id + id >< k) . (id + split f id) . outNat
|
\just\equiv{ shunt-left, shunt-right }
|
     k . inNat = inList . (id + id >< k) . (id + split f id)
|
\just\equiv{ functor-|+| }
|
     k . either (const 0) succ = either nil cons . (id + (id >< k) . split f id)
|
\just\equiv{ absorção-|><|, absorção-|+| }
|
     k . either (const 0) succ = either nil (cons . split f k)
|
\just\equiv{ fusão-|+|, eq-|+|, pointwise }
|
lcbr(
     k 0 = []
)(
     k (n+1) = f n : k n
)
|
\end{eqnarray*}


\noindent {\large \textbf{Exercício 5}}
\begin{eqnarray*}
\start
|
     suffixes = anaList ((id + split cons p2) . outList)
|
\just\equiv{ universal-ana }
|
     outList . suffixes = (id + id >< suffixes) . (id + split cons p2) . outList
|
\just\equiv{ shunt-left, shunt-right }
|
     suffixes . inList = inList . (id + id >< suffixes) . (id + split cons p2)
|
\just\equiv{ functor-|+|, fusão-|+| }
|
     either (suffixes . nil) (suffixes . cons) = either nil cons . (id + ((id >< suffixes) . split cons p2))
|
\just\equiv{ absorção-|+| }
|
     either (suffixes . nil) (suffixes . cons) = either nil (cons . ((id >< suffixes) . split cons p2))
|
\just\equiv{ absorção-|><| }
|
     either (suffixes . nil) (suffixes . cons) = either nil (cons . split cons (suffixes . p2))
|
\just\equiv{ eq-|+|, pointwise }
|
lcbr(
     suffixes [] = []
)(
     suffixes (h:t) = (h:t) : suffixes t
)
|
\end{eqnarray*}

\noindent {\large \textbf{Exercício 6}}
\begin{eqnarray*}
\start
|
     cataList (either (const 0) (succ . p2)) = anaNat ((id + p2) . outList)
|
\just\equiv{ universal-ana }
|
     outNat . cataList (either (const 0) (succ . p2)) = fF cataList (either (const 0) (succ . p2)) . (id + p2) . outList
|
\just\equiv{ shunt-left, shunt-right, def. functor dos naturais }
|
     cataList (either (const 0) (succ . p2)) . inList = inNat . (id + cataList (either (const 0) (succ . p2))) . (id + p2)
|
\just\equiv{ functor-|+|, def. |inNat|, cancelamento-cata }
|
     either (const 0) (succ . p2) . (id + id >< cataList (either (const 0) (succ . p2))) = either (const 0) succ . (id + cataList (either (const 0) (succ . p2)) . p2)
|
\just\equiv{ absorção-|+| }
|
     either (const 0) (succ . p2 . (id >< cataList (either (const 0) (succ . p2)))) = either (const 0) (succ . cataList (either (const 0) (succ . p2)) . p2)
|
\just\equiv{ eq-|+|, natural-|p2| }
|
lcbr(
     const 0 = const 0
)(
     succ . cataList (either (const 0) (succ . p2)) . p2 = succ . cataList (either (const 0) (succ . p2)) . p2
)
|
\qed
\end{eqnarray*}



\noindent {\large \textbf{Exercício 7}}
\begin{code}
Data QTree a = Pixel a | Blocks ((QTree a, QTree a), (QTree a, QTree a))
     deriving (Show)

inQTree :: Either a ((QTree a, QTree a), (QTree a, QTree a)) -> QTree a
inQTree = either Pixel Blocks

outQTree :: QTree a -> Either a ((QTree a, QTree a), (QTree a, QTree a))
outQTree (Pixel x) = i1 x
outQTree (Blocks ((x, y), (z, w))) = i2 ((x, y), (z, w))

-- Bi-Functor de QTree
baseQTree g f = g -|- ((f >< f) >< (f >< f))

-- Functor de QTree
recQTree f = baseQTree id f

-- catamorfismo de QTree
cataQTree g = g . (recQTree (cataQTree g)) . outQTree

-- anamorfismo de QTree
anaQTree g = inQTree . (recQTree (anaQTRee g)) . g     

-- hylomorfismo de QTree
hyloQTree f g = cataQTree f . anaQTree g

instance Functor QTree where
     fmap f = cataQTree (inQTree . baseQTree f id)

mirrorQTree = cataQTree (either Pixel (Blocks . swap . (swap >< swap)))

countQTree = cataQTree (either one (add . (add >< add)))

depthQTree = cataQTree (either one (succ . umax . (umax >< umax)))

rotate90 = cataQTree (either Pixel (Blocks . f))
     where f ((x, y), (z, w)) = ((w, x), (y, z))

tips = cataQTree (either singl (conc . (conc >< conc)))
\end{code}



\end{document}
%----------------- Fim do documento -------------------------------------------%
