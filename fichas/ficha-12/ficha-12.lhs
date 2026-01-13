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
%format (sub (t) (p)) = t "_{" p "}"
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
%format (floor(f)) = "\lfloor" f "\rfloor"
%format f1 = " f_1"
%format f2 = " f_2"
%==============================================================================%


\title{Cálculo de Programas \\ Resolução - Ficha 12}
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

\begin{minipage}{0.4\textwidth}
\begin{eqnarray*}
\start
|
     muB = kcomp id id
|
\just\equiv{ (F6) }
|
     muB = muB . T id . id
|
\just\equiv{ natural-|id|, functor-|id|-|T| (46) }
|
     muB = muB . id
|
\just\equiv{ natural-|id| }
|
     muB = muB
|
\qed
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.6\textwidth}
\begin{eqnarray*}
\start
|
     kcomp ((f . g)) h = kcomp f (((T g) . h))
|
\just\equiv{ (F6) (twice) }
|
     muB . T (f . g) . h = muB . (T f) . (T g) . h
|
\just\equiv{ functor-|T| (45) }
|
     muB . (T f) . (T g) . h = muB . (T f) . (T g) . h
|
\qed
\end{eqnarray*}
\end{minipage}


\begin{minipage}{0.5\textwidth}
\begin{eqnarray*}
\start
|
     kcomp f u = f
|
\just\equiv{ (F6) }
|
     muB . T f . u = f
|
\just\equiv{ (F4) }
|
     muB . u . f = f
|
\just\equiv{ (F2) }
|
     id . f = f
|
\just\equiv{ natural-|id| }
|
     f = f
|
\qed
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.5\textwidth}
\begin{eqnarray*}
\start
|
     f = kcomp u f
|
\just\equiv{ (F6) }
|
     f = muB . T u . f
|
\just\equiv{ (F2) }
|
     f = id . f
|
\just\equiv{ natural-|id| }
|
     f = f
|
\qed
\end{eqnarray*}
\end{minipage}

\begin{eqnarray*}
\start
|
     T f = kcomp ((u . f)) id
|
\just\equiv{ (F6) }
|
     T f = muB . T (u . f) . id
|
\just\equiv{ natural-|id|, functor-|T| (45) }
|
     T f = muB . (T u) . (T f)
|
\just\equiv{ (F2), natural-|id| }
|
     T f = T f
|
\qed
\end{eqnarray*}


\noindent {\large \textbf{Exercício 2}}
\begin{eqnarray*}
\start
|
     discollect = kcomp lstr id
|
\just\equiv{ composição monádica }
|
     discollect = concat . T lstr . id
|
\just\equiv{ natural-|id|, def. |concat|, absorção-|cata| }
|
     discollect = cata (either nil conc . B (lstr, id))
|
\just\equiv{ universal-cata, def. bi-functor de listas }
|
     discollect . either nil cons = either nil conc . (id + lstr >< id) . (id + id >< discollect)
|
\just\equiv{ fusão-|+|, absorção-|+| (twice), eq-|+| }
|
lcbr(
     discollect . nil = nil
)(
     discollect . cons = conc . (lstr >< id) . (id >< discollect)
)
|
\just\equiv{ functor-|><| }
|
lcbr(
     discollect . nil = nil
)(
     discollect . cons = conc . (lstr >< discollect)
)
|
\just\equiv{ pointwise, def. |conc|, def. |lstr| }
|
lcbr(
     discollect [] = []
)(
     discollect ((a,l):as) = t ++ discollect as where t = [(a,b) || b <- l ]
)
|
\end{eqnarray*}

\noindent {\large \textbf{Exercício 3}}
\begin{eqnarray*}
\start
|
     muB . muB = muB . T muB
|
\just\equiv{ pointwise, def. comp }
|
     muB (muB (((x, y), z), w)) = muB ((muB >< id) (((x,y),z),w))
|
\just\equiv{ def.|muB|, def.|><|, def.|id| }
|
     muB ((x,y), z+w) = muB ((x,y+z),w)
|
\just\equiv{ def.|muB| }
|
     (x,y + z + w) = (x,y + z + w)
|
\qed
\end{eqnarray*}

\begin{minipage}{0.4\textwidth}
\begin{eqnarray*}
\start
|
     muB . u = id
|
\just\equiv{ pointwise, def. comp }
|
     muB (u (x,y)) = id (x,y)
|
\just\equiv{ def. |u|, def. id }
|
     muB ((x,y), 0) = (x,y)
|
\just\equiv{ def.|muB| }
|
     (x,y+0) = (x,y)
|
\qed
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.6\textwidth}
\begin{eqnarray*}
\start
|
     muB . T u = id
|
\just\equiv{ pointwise, def. comp, def. |T u| }
|
     muB ((u >< id) (x,y)) = id (x,y)
|
\just\equiv{ def-|><|, def. |u|, def. |id| }
|
     muB ((x,0),y) = (x,y)
|
\just\equiv{ def.|muB| }
|
     (x, 0+y) = (x,y)
|
\qed
\end{eqnarray*}
\end{minipage}


\newpage

\noindent {\large \textbf{Exercício 4}}

\begin{minipage}{0.5\textwidth}
\begin{eqnarray*}
\start
|
     muB . T u = id
|
\just\equiv{ def. |muB| }
|
     cata (either id (inList . i2)) . T u = id
|
\just\equiv{ absorção-cata }
|
     cata (either id (inList . i2) . B (u, id)) = id
|
\just\equiv{ universal-cata, def. bi-functor B, natural-|id| }
|
     either id (inList . i2) . (u + G id) . F id = inList
|
\just\equiv{ functor-|id|-|F| (twice), absorção-|+| }
|
     either u (inList . i2) = inList
|
\just\equiv{ def. |u| }
|
     either (inList . i1) (inList . i2) = inList
|
\just\equiv{ fusão-|+|, reflexão-|+| }
|
     inList = inList
|
\qed
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.5\textwidth}
\begin{eqnarray*}
\start
|
     muB . u = id
|
\just\equiv{ def. |muB|, def. |u| }
|
     cata (either id (inList . i2)) . inList . i1 = id
|
\just\equiv{ cancelamento-cata }
|
     either id (inList . i2) . fF muB . i1 = id
|
\just\equiv{ base-cata, def. functor |fF| }
|
     either id (inList . i2) . (id + G muB) . i1 = id
|
\just\equiv{ absorção-|+| }
|
     either id (inList . i2 . G muB) . i1 = id
|
\just\equiv{ cancelamento-|+| }
|
     id = id
|
\qed
\end{eqnarray*}
\end{minipage}

\begin{eqnarray*}
\start
|
     muB . muB = muB . T muB
|
\just\equiv{ def. |muB| }
|
     muB . muB = cata (either id (inList . i2)) . T muB
|
\just\equiv{ absorção-cata, def. |muB| }
|
     muB . cata (either id (inList . i2)) = cata (either id (inList . i2) . B (muB, id))
|
\just\impliedby{ fusão-cata }
|
     muB . either id (inList . i2) = either id (inList . i2) . B (muB, id) . fF muB
|
\just\equiv{ fusão-|+|, |fF f = B (id, f)|, |B (f,g) = f + G g| (twice), absorção-|+| }
|
     either muB (muB . inList . i2) = either muB (inList . i2 . G muB)
|
\just\equiv{ eq-|+| }
|
lcbr(
     muB = muB
)(
     muB . inList . i2 = inList . i2 . G muB
)
|
\just\equiv{ def. |muB|, cancelamento-cata }
|
lcbr(
     true
)(
     either id (inList . i2) . fF muB . i2 = inList . i2 . G muB
)
|
\just\equiv{ |fF f = id + G f|, absorção-|+|, cancelamento-|+| }
|
lcbr(
     true
)(
     inList . i2 . G muB = inList . i2 . G muB
)
|
\qed
\end{eqnarray*}

\newpage

\noindent {\large \textbf{Exercício 5}}
\begin{eqnarray*}
\xymatrix@@C=3cm@@R=1cm{
     |A|
          \ar[r]^-{| inLTree . i1 |}
&
     |LTree A|
&
     |LTree (LTree A)|
          \ar[l]_-{| cata (either id (inLTree . i2)) |}
}
\end{eqnarray*}

\noindent O Functor Base de LTree é |B (X, Y) = X + Y >< Y|, logo podemos deduzir o functor G como |G Y = Y >< Y|.\\
     
\noindent Para |G Y = 1| temos o Functor Base de Maybe |B (X,Y) = X + 1|.\\
     
\noindent Para |G Y = O >< Seq(Y)| temos o Functor Base de Árvores de Expressão |B (X,Y) = X + O >< Seq(Y)| (presente na biblioteca \Exp).\\

\noindent {\large \textbf{Exercício 6}}
\begin{eqnarray*}
\start
|
     sequence = cata (either return id . (nil + floor cons))
|
\just\equiv{ universal-cata }
|
     sequence . either nil cons = either return id . (nil + floor cons) . (id + id >< sequence)
|
\just\equiv{ fusão-|+|, absorção-|+| (twice), eq-|+| }
|
lcbr(
     sequence . nil = return . nil
)(
     sequence . cons = floor cons . (id >< sequence)
)
|
\just\equiv{ pointwise, def-|><| }
|
lcbr(
     sequence [] = return []
)(
     sequence (h : t) = floor cons (h, sequence t)
)
|
\just\equiv{ def. |floor f|, def. |cons| }
|
lcbr(
     sequence [] = return []
)(
     sequence (h:t) = do { a <- h ; b <- sequence t; return (a:b) }
)
|
\end{eqnarray*}


\end{document}
%----------------- Fim do documento -------------------------------------------%
