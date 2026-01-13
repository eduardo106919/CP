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


\title{Cálculo de Programas \\ Resolução - Ficha 01}
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

\begin{minipage}{0.3\textwidth}
\begin{eqnarray*}
\start
|
     p1 . (f >< g) (x,y)
|
\just\equiv{ Def. comp }
|
     p1 ((f >< g) (x,y))
|
\just\equiv{ (F1) }
|
     p1 (f x, g y)
|
\just\equiv{ (F2) }
|
     f x
|
\just\equiv{ (F2) }
|
     f (p1 (x,y))
|
\just\equiv{ Def. comp }
|
     f . p1
|
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.2\textwidth}
\begin{eqnarray*}
\start
|
     p2 . (f >< g) (x,y)
|
\just\equiv{ Def. comp }
|
     p2 ((f >< g) (x,y))
|
\just\equiv{ (F1) }
|
     p2 (f x, g y)
|
\just\equiv{ (F2) }
|
     g y
|
\just\equiv{ (F2) }
|
     g (p2 (x,y))
|
\just\equiv{ Def. comp }
|
     g . p2
|
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.3\textwidth}
\begin{eqnarray*}
\start
|
     (f >< g) (x,y)
|
\just\equiv{ (F1) }
|
     (f x, g y)
|
\just\equiv{ (F2) }
|
     (f (p1 (x,y)), g (p2 (x,y)))
|
\just\equiv{ Def. comp }
|
     (f . p1, g . p2)
|
\just\equiv{ Def. split }
|
     split (f . p1) (g . p2)
|
\end{eqnarray*}
\end{minipage}

\noindent {\large \textbf{Exercício 2}}
\begin{eqnarray*}
\start
|
     xor . (and >< id) ((a,b), c)
|
\just\equiv{ Def. comp }
|
     xor ((and >< id) ((a,b), c))
|
\just\equiv{ (F1) }
|
     xor (and (a,b), id c)
|
\just\equiv{ Def. |and|, Def. |id| }
|
     xor (a && b, c)
|
\just\equiv{ Def. |xor| }
|
     (a && b)| \oplus |c
|
\end{eqnarray*}

\noindent {\large \textbf{Exercício 4}}
\begin{eqnarray*}
\start
|
     id = split f g
|
\just\equiv{ universal-|><| }
|
lcbr(
     p1 . id = f
)(
     p2 . id = g
)
|
\just\equiv{ natural-|id| }
|
lcbr(
     p1 = f
)(
     p2 = g
)
|
\end{eqnarray*}

\noindent Concluimos então que |id = split p1 p2|. Seja |k = id|, ao aplicar a propriedade universal-|><| obtemos a propriedade reflexão-|><|.

\noindent {\large \textbf{Exercício 5}}
\begin{eqnarray*}
\start
|
     split h k . f = split (h . f) (k . f)
|
\just\equiv{ (F7) }
|
lcbr(
     p1 . split h k . f = h . f
)(
     p2 . split h k .f = k . f
)
|
\just\equiv{ cancelamento-|><| }
|
lcbr(
     h . f = h . f
)(
     k . f = k . f
)
|
\qed
\end{eqnarray*}

\noindent {\large \textbf{Exercício 6}}
\begin{eqnarray*}
\start
|
     dup . f = split f f
|
\just\equiv{ pointwise, Def. comp }
|
     dup (f x) = split f f x
|
\just\equiv{ Def. dup, Def. split }
|
     (f x, f x) = (f x, f x)
|
\qed
\end{eqnarray*}

\noindent {\large \textbf{Exercício 7}}
\begin{eqnarray*}
\start
|
     const (b,a) = split (const b) (const a)
|
\just\equiv{ universal-|><| }
|
lcbr(
     p1 . const ((b,a)) = const b
)(
     p2 . const ((b,a)) = const a
)
|
\just\equiv{ absorção-const }
|
lcbr(
     const (p1 (b,a)) = const b
)(
     const (p2 (b,a)) = const a
)
|
\just\equiv{ cancelamento-|><| }
|
lcbr(
     const b = const b
)(
     const a = const a
)
|
\qed
\end{eqnarray*}


\noindent {\large \textbf{Exercício 8}}
\begin{eqnarray*}
\start
|
     (g >< f) . swap = swap . (f >< g)
|
\just\equiv{ Def-|><|, Def. |swap| }
|
     split (g . p1) (f . p2) . swap = split p2 p1 . (f >< g)
|
\just\equiv{ fusão-|><| (twice) }
|
     split (g . p1 . swap) (f . p2 . swap) = split (p2 . (f >< g)) (p1 . (f >< g))
|
\just\equiv{ Def. |swap|, Def-|><| }
|
     split (g . p2 . split p2 p1) (f . p2 . split p2 p1) = split (p2 . split (f . p1) (g . p2)) (p1 . split (f . p1) (g . p2))
|
\just\equiv{ cancelamento-|><| (twice) }
|
     split (g . p2) (f . p1) = split (g . p2) (f . p1)
|
\qed
\end{eqnarray*}

\noindent {\large \textbf{Exercício 9}}
\begin{code}
acronym = map head . words
\end{code}
\begin{eqnarray*}
\xymatrix@@C=2cm{
    |String|
           \ar[r]^-{|words|}
&
     |String|^*
           \ar[r]^-{|map head|}
&
     |String|
}
\end{eqnarray*}

\begin{code}
short = uncurry (++) . (id >< (' ':)) . split head last . words
\end{code}
\begin{eqnarray*}
\xymatrix@@C=2cm{
&
     |String|
          \ar[d]^-{|words|}
&
\\
&
     |String|^*
          \ar[dl]_-{|head|}
          \ar[dr]^-{|last|}
&
\\
     |String|
          \ar[d]_-{|id|}
&
&
     |String|
          \ar[d]^-{|(' ':)|}
\\
     |String|
          \ar@@{-}[r]
&
          \ar[d]^-{|uncurry (++)|}
&
     |String|
          \ar@@{-}[l]
\\
&
     |String|
&
}
\end{eqnarray*}


\end{document}
%----------------- Fim do documento -------------------------------------------%
