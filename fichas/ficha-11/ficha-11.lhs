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
%format f1 = " f_1"
%format f2 = " f_2"
%==============================================================================%


\title{Cálculo de Programas \\ Resolução - Ficha 11}
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
\start
|
     mirror = cata (sub(inLTree)(2) . alfa)
|
\just\equiv{ def. |sub(inLTree)(2)|, def. |alfa| }
|
     mirror = cata (either Leaf Fork . (id + swap))
|
\just\equiv{ absorção-|+| }
|
     mirror = cata (either Leaf (Fork . swap))
|
\just\equiv{ universal-cata }
|
     mirror . inLTree = either Leaf (Fork . swap) . (id + (mirror >< mirror))
|
\just\equiv{ def. |inLTree|, fusão-|+|, absorção-|+|, eq-|+| }
|
lcbr(
     mirror . Leaf = Leaf
)(
     mirror . Fork = Fork . swap . (mirror >< mirror)
)
|
\just\equiv{ pointwise, Def. comp }
|
lcbr(
     mirror (Leaf x) = Leaf x
)(
     mirror (Fork (l,r)) = Fork (swap (mirror l, mirror r))
)
|
\just\equiv{ def. |swap| }
|
lcbr(
     mirror (Leaf x) = Leaf x
)(
     mirror (Fork (l,r)) = Fork (mirror r, mirror l)
)
|
\end{eqnarray*}


\noindent {\large \textbf{Exercício 2}}
\begin{eqnarray*}
\start
|
     cata g . cata (sub(inLTree)(2) . alfa) = cata (g . alfa)
|
\just\impliedby{ fusão-cata }
|
     cata g . sub(inLTree)(2) . alfa = g . alfa . fF (cata g)
|
\just\equiv{ (F1) }
|
     g . G (cata g) . alfa = g . alfa . fF (cata g)
|
\just\impliedby{ leibniz }
|
     G (cata g) . alfa = alfa . fF (cata g)
|
\just\impliedby{ generalização de |cata g| em |f| }
|
     G f . alfa = alfa . fF f
|
\end{eqnarray*}

\noindent {\large \textbf{Exercício 3}}
\begin{eqnarray*}
\start
\begin{array}{c}
| mirror = cata g | \\
| mirror = cata (sub(inLTree)(2) . alfa) |
\end{array}
\end{eqnarray*}
\begin{eqnarray*}
\start
|
     cata g = cata (sub(inLTree)(2) . alfa)
|
\just\equiv{ def. |sub(inLTree)(2)|, def. |alfa| }
|
     cata g = cata (either Leaf Fork . (id + swap))
|
\just\equiv{ absorção-|+| }
|
     cata g = cata (either Leaf (Fork . swap))
|
\end{eqnarray*}

\noindent Podemos então dizer que |g = either Leaf (Fork . swap)|. Precisamos também de provar que |id = cata (g . alfa)|.
\begin{eqnarray*}
\start
|
     id = cata (g . alfa)
|
\just\equiv{ def. |g|, def. |alfa| }
|
     id = cata (either Leaf (Fork . swap) . (id + swap))
|
\just\equiv{ absorção-|+| }
|
     id = cata (either Leaf (Fork . swap . swap))
|
\just\equiv{ |swap . swap = id| }
|
     id = cata (either Leaf Fork)
|
\just\equiv{ |either Leaf Fork = sub(inLTree)(LTree)| }
|
     id = cata (sub(inLTree)(LTree))
|
\just\equiv{ reflexão-cata }
|
     id = id
|
\qed
\end{eqnarray*}

Podemos então provar que |mirror| é o seu próprio isomorfismo:
\begin{eqnarray*}
\start
|
     cata g . cata (sub(inLTree)(2) . alfa) = cata (g . alfa)
|
\just\impliedby{ (F3) }
|
     G f . alfa = alfa . fF f
|
\just\equiv{ |G f = fF f = id + f >< f|, def. |alfa| (twice) }
|
     (id + f >< f) . (id + swap) = (id + swap) . (id + f >< f)
|
\just\equiv{ functor-|+| }
|
     (id + ((f >< f) . swap)) = (id + (swap . (f >< f)))
|
\qed
\end{eqnarray*}

\noindent Através da propriedade grátis da função |swap| (i.e: |swap . (f >< g) = (g >< f) . swap|), podemos garantir a veracidade desta propriedade.

\noindent {\large \textbf{Exercício 4}}
\begin{eqnarray*}
\start
|
     cata g . T f = cata (g . B (f, id))
|
\just\equiv{ def-map-cata }
|
     cata g . cata (inNat . B (f, id)) = cata (g . B (f, id))
|
\just\impliedby{ (F3) }
|
     G f . B (f, id) = B (f, id) . fF f
|
\just\equiv{ |G f = B (id, f)| }
|
     B (id, f) . B (f, id) = B (f, id) . B (id, f)
|
\just\equiv{ functor-|id|-|fF| (para bi-functores) }
|
     B (f, f) = B (f, f)
|
\qed
\end{eqnarray*}


\noindent {\large \textbf{Exercício 5}}
\begin{eqnarray*}
\start
|
     while p f g = tailr ( (g + f) . (not . p) ?)
|
\just\equiv{ def. |tailr| }
|
     while p f g = hylo join ((g + f) . (not . p) ?)
|
\just\equiv{ |hylo f g = f . fF (hylo f g) . g| }
|
     while p f g = join . fF (while p f g) . ((g + f) . (not . p) ?)
|
\just\equiv{ def. |join|, def. functor F }
|
     while p f g = either id id . (id + (while p f g)) . ((g + f) . (not . p) ?)
|
\just\equiv{ absorção-|+| (twice) }
|
     while p f g = either g ((while p f g) . f) . (not . p) ?
|
\just\equiv{ def. condicional de McCarthy }
|
     while p f g = Cp.cond ((not . p)) g ((while p f g ) . f)
|
\just\equiv{ pointwise }
|
     while p f g x = if not (p x) then g x else while p f g (f x)
|
\end{eqnarray*}


\noindent {\large \textbf{Exercício 6}}
\begin{eqnarray*}
\start
|
     (tailr g) . f = tailr h
|
\just\equiv{ def. |tailr|, def. hilomorfismo }
|
     cata delta . ana g = cata delta . ana h
|
\just\impliedby{ leibniz }
|
     ana g . f = ana h
|
\just\impliedby{ fusão-ana, def. functor }
|
     g . f = (id + f) . h
|
\qed
\end{eqnarray*}


\noindent {\large \textbf{Exercício 7}}
\begin{eqnarray*}
\start
|
     kcomp f (either g h) = either (kcomp f g) (kcomp f h)
|
\just\equiv{ (F9) (3*) }
|
     muB . T f . either g h = either (muB . T f . g) (muB . T f . h)
|
\just\equiv{ fusão-|+| }
|
     either (muB . T f . g) (muB . T f . h) = either (muB . T f . g) (muB . T f . h)
|
\qed
\end{eqnarray*}


\end{document}
%----------------- Fim do documento -------------------------------------------%
