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


\title{Cálculo de Programas \\ Resolução - Ficha 08}
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

\begin{minipage}{0.3\textwidth}
\begin{eqnarray*}
\start
|
     T id = id
|
\just\equiv{ (F1) }
|
     id >< id = id
|
\just\equiv{ def-|><| }
|
     split (id . p1) (id . p2) = id
|
\just\equiv{ natural-|id| (twice) }
|
     split p1 p2 = id
|
\just\equiv{ reflexão-|><| }
|
     id = id
|
\qed
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.6\textwidth}
\begin{eqnarray*}
\start
|
     T (f . g) = (T f) . (T g)
|
\just\equiv{ (F1) }
|
     (f . g) >< (f . g) = (f >< f) . (g >< g)
|
\just\equiv{ def-|><| (twice) }
|
     split (f . g . p1) (f . g . p2) = split (f . p1) (f . p2) . (g >< g)
|
\just\equiv{ fusão-|><| }
|
     split (f . g . p1) (f . g . p2) = split (f . p1 . (g >< g)) (f . p2 . (g >< g))
|
\just\equiv{ natural-|p1|, natural-|p2| }
|
     split (f . g . p1) (f . g . p2) = split (f . g . p1) (f . g . p2)
|
\qed
\end{eqnarray*}
\end{minipage}

\begin{eqnarray*}
\start
|
     muB . (T u) = muB . u
|
\just\equiv{ def. |muB|, def. |T u| }
|
     (p1 >< p2) . (split id id >< split id id) = (p1 >< p2) . split id id
|
\just\equiv{ def-|><|, absorção-|><|, natural-|id| }
|
     (p1 >< p2) . split (split id id . p1 (split id id . p2) = split p1 p2
|
\just\equiv{ absorção-|><|, reflexão-|><| }
|
     split (p1 . split id id . p1) (p2 . split id id . p2) = split p1 p2
|
\just\equiv{ cancelamento-|><| }
|
     split p1 p2 = split p1 p2
|
\qed
\end{eqnarray*}


\noindent {\large \textbf{Exercício 2}}
\begin{eqnarray*}
\start
|
     loop (a,b) = (2+b, 2-a)
|
\just\equiv{ pointfree }
|
     loop = ((2+) >< (2-)) . swap
|
\end{eqnarray*}
\begin{eqnarray*}
\start
|
     split f g = for loop (4,-2)
|
\just\equiv{ def. for }
|
     split f g = cataNat (either (const ((4,-2))) loop)
|
\just\equiv{ def. |loop| }
|
     split f g = cataNat (either (split (const 4) (const (-2))) ((2+) >< (2-) . swap) )
|
\just\equiv{ def-|><|, fusão-|><| }
|
     split f g = cataNat (either (split (const 4) (const (-2))) (split ((2+) . p1 . swap) ((2-) . p2 . swap) ) )
|
\just\equiv{ lei da troca }
|
     split f g = cataNat (split (either (const 4) ((2+) . p1 . swap) ) (either (const (-2)) ((2-) . p2 . swap) ) )
|
\just\equiv{ fokkinga }
|
lcbr(
     f . either (const 0) succ = either (const 4) ((2+) . p1 . swap) . (id + split f g)
)(
     g . either (const 0) succ = either (const (-2)) ((2-) . p2 . swap) . (id + split f g)
)
|
\just\equiv{ fusão-|+|, absorção-|+|, eq-|+| }
|
lcbr4(
     f . const 0 = const 4
)(
     f . succ = (2+) . p1 . swap
)(
     g . const 0 = const (-2)
)(
     g . succ = (2-) . p2 . swap
)
|
\just\equiv{ pointwise }
|
lcbr4(
     f 0 = 4
)(
     f (n+1) = 2 + g n
)(
     g 0 = -2
)(
     g (n+1) = 2 - f n
)
|
\end{eqnarray*}


\noindent {\large \textbf{Exercício 3}}
\begin{eqnarray*}
\start
|
     split (split f g) j = cata (split (split h k) l)
|
\just\equiv{ fokkinga }
|
lcbr(
     split f g . inList = split h k . fF (split (split f g) h)
)(
     j . inList = l . fF (split (split f g) h)
)
|
\just\equiv{ fusão-|><| (twice) }
|
lcbr(
     split (f . inList) (g . inList) = split (h . fF (split (split f g) j)) (k . fF (split (split f g) j))
)(
     j . inList = l . fF (split (split f g) j)
)
|
\just\equiv{ eq-|><| }
|
lcbr3(
     f . inList = h . fF (split (split f g) j)
)(
     g . inList = k . fF (split (split f g) j)
)(
     j . inList = l . fF (split (split f g) j)
)
|
\end{eqnarray*}


\noindent {\large \textbf{Exercício 4}}
\begin{eqnarray*}
\start
|
lcbr4(
     impar 0 = false
)(
     impar (n+1) = par n
)(
     par 0 = true
)(
     par (n+1) = impar n
)
|
\just\equiv{ pointfree }
|
lcbr4(
     impar . const 0 = const false
)(
     impar . succ = par
)(
     par . const 0 = const true
)(
     par . succ = impar
)
|
\just\equiv{ eq-|+| }
|
lcbr(
     either (impar . const 0) (impar . succ) = either (const false) par
)(
     either (par . const 0) (par . succ) = either (const true) impar
)
|
\just\equiv{ fusão-|+|, cancelamento-|+| (twice) }
|
lcbr(
     impar . inNat = either (const false) (p2 . split impar par)
)(
     par . inNat = either (const true) (p1 . split impar par)
)
|
\just\equiv{ natural-|id| (twice), absorção-|+| }
|
lcbr(
     impar . inList = either (const false) p2 . (id + split impar par)
)(
     par . inList = either (const true) p1 . (id + split impar par)
)
|
\end{eqnarray*}

\noindent Podemos então concluir que |h = either (const false) p2| e |k = either (const true) p1|.
\begin{eqnarray*}
\start
|
     split impar par = for swap (false, true)
|
\just\equiv{ def. for }
|
     split impar par = cataNat (either (split (const false) (const true)) swap)
|
\just\equiv{ def. |swap| }
|
     split impar par = cataNat (either (split (const false) (const true)) (split p2 p1))
|
\just\equiv{ lei da troca }
|
     split impar par = cataNat (split ( either (const false) (p2) ) (either (const true) (p1) ) )
|
\just\equiv{ fokkinga }
|
lcbr(
     impar . either (const 0) succ = either (const false) p2 . (id + split impar par)
)(
     par . either (const 0) succ = either (const true) p1 . (id + split impar par)
)
|
\just\equiv{ fusão-|+|, absorção-|+| }
|
lcbr(
     either (impar . const 0) (impar . succ) = either (const false) (p2 . split impar par)
)(
     either (par . const 0) (impar . succ) = either (const true) (p1 . split impar par)
)
|
\just\equiv{ cancelamento-|><| (twice), eq-|+|, pointwise }
|
lcbr4(
     impar 0 = false
)(
     impar (n+1) = par n
)(
     par 0 = true
)(
     par (n+1) = impar n
)
|
\end{eqnarray*}

\noindent {\large \textbf{Exercício 5}}
\begin{eqnarray*}
\start
|
lcbr4(
     insg 0 = []
)(
     insg (n+1) = fsuc n : insg n
)(
     fsuc 0 = 1
)(
     fsuc (n+1) = fsuc n +1
)
|
\just\equiv{ pointfree }
|
lcbr4(
     insg . const 0 = nil
)(
     insg . succ = cons . split fsuc insg
)(
     fsuc . const 0 = const 1
)(
     fsuc . succ 0 = (1+) . fsuc
)
|
\just\equiv{ cancelamento-|><| }
|
lcbr4(
     insg . const 0 = nil
)(
     insg . succ = cons . split fsuc insg
)(
     fsuc . const 0 = const 1
)(
     fsuc . succ 0 = (1+) . p1 . split fsuc insg
)
|
\just\equiv{ eq-|+|, fusão-|+|, absorção-|+| }
|
lcbr(
     insg . inNat = either nil cons . (id + split fsuc insg)
)(
     fsuc . inNat = either (const 1) ((1+) . p1) . (id + split fsuc insg)
)
|
\just\equiv{ fokkinga }
|
     split fsuc insg = cataNat (split inList (either (const 1) ((1+) . p1)) )
|
\end{eqnarray*}

\begin{eqnarray*}
\start
|
     insgfor = for (split ((1+) . p1) cons) (split (const 1) (nil))
|
\just\equiv{ def. for }
|
     insgfor = cataNat ( either ( split (const 1) nil ) ( split ((1+) . p1) cons ) )
|
\just\equiv{ lei da troca }
|
     insgfor = cataNat ( split ( either (const 1) ((1+) . p1) ) (either nil cons ) )
|
\just\equiv{  }
|
     split f insg = cataNat ( split ( either (const 1) ((1+) . p1) ) (either nil cons ) )
|
\just\equiv{ fokkinga }
|
lcbr(
     f . either (const 0) succ = either (const 1) ((1+) . p1) . fF split f insg
)(
     insg . either (const 0) succ = either nil cons . fF split f insg
)
|
\just\equiv{ def. functor |Nat0|, fusão-|+|, absorção-|+|, eq-|+| }
|
lcbr4(
     f . const 0 = const 1
)(
     f . succ = (1+) . p1 . split f insg
)(
     insg . const 0 = nil
)(
     insg . succ = cons . split f insg
)
|
\just\equiv{ pointwise }
|
lcbr4(
     f 0 = 1
)(
     f (n+1) = 1 + f n
)(
     insg 0 = []
)(
     insg (n+1) = f n : insg n
)
|
\end{eqnarray*}


\noindent {\large \textbf{Exercício 6}}
\begin{eqnarray*}
\start
|
lcbr4(
     f1 [] = []
)(
     f1 (h:t) = h : f2 t
)(
     f2 [] = []
)(
     f2 (h:t) = f1 t
)
|
\just\equiv{ def. comp, pointfree }
|
lcbr4(
     f1 . nil = nil
)(
     f1 . cons = cons . (id >< f2)
)(
     f2 . nil = nil
)(
     f2 . cons = f1 . p2
)
|
\just\equiv{ eq-|+|, fusão-|+| }
|
lcbr(
     f1 . either nil cons = either nil (cons . (id >< f2))
)(
     f2 . either nil cons = either nil (f1 . p2)
)
|
\just\equiv{ def. |inList|, natural-|id| (twice), natural-|p2|, cancelamento-|><| }
|
lcbr(
     f1 . inList = either (nil . id) (cons . (id >< p2 . split f1 f2))
)(
     f2 . inList = either (nil . id) (p2 . (id >< p1 . split f1 f2))
)
|
\just\equiv{ functor-|><| }
|
lcbr(
     f1 . inList = either (nil . id) (cons . (id >< p2) . (id >< split f1 f2))
)(
     f2 . inList = either (nil . id) (p2 . (id >< p1) . (id >< split f1 f2))
)
|
\just\equiv{ absorção-|+|, def. functor de listas, natural-|p2| }
|
lcbr(
     f1 . inList = either nil (cons . (id >< p2)) .fF (split f1 f2)
)(
     f2 . inList = either nil (p1 . p2) . fF (split f1 f2)
)
|
\just\equiv{ fokkinga }
|
     split f1 f2 = cataList (split (inList . (id >< p2)) (either nil (p1 . p2)) )
|
\end{eqnarray*}
\begin{eqnarray*}
\xymatrix@@C=3cm@@R=1cm{
    |Seq(A)|
           \ar[d]_-{|split f1 f2|}
           \ar@@/^1pc/[r]^-{|outList|}
&
    |1 + A >< Seq(A)|
           \ar[d]^{|id + id >< split f1 f2|}
           \ar@@/^1pc/[l]^-{|inList|}
\\
     |Seq(A) >< Seq(A)|
&
     |1 + A >< (Seq(A) >< Seq(A))|
           \ar@@/^1pc/[l]^-{|g|}
}
\end{eqnarray*}

\noindent Diagrama do gene do catamorfismo:
\begin{eqnarray*}
\xymatrix@@C=3cm@@R=1cm{
&
    |1 + A >< (Seq(A) >< Seq(A))|
          \ar[dr]^-{|id + p2|}
          \ar[dl]_-{|id + (id >< p2)|}
          \ar[dd]_-{|g|}
&
\\
     |1 + (A >< Seq(A))|
          \ar[d]_-{|inList|}
&
&
     |1 + (Seq(A) >< Seq(A))|
          \ar[d]^-{|either nil p1|}
\\
     |Seq(A)|
&
     |Seq(A) >< Seq(A)|
          \ar[r]_-{|p2|}
          \ar[l]^-{|p1|}
&
     |Seq(A)|
}
\end{eqnarray*}

\noindent A função |f1| seleciona os elementos de uma lista nas posições pares, e a função |f2| seleciona os elementos de uma lista nas posições ímpares.

\noindent {\large \textbf{Exercício 7}}

\begin{minipage}{0.5\textwidth}
\begin{eqnarray*}
\start
|
     H (g . h) = (H g) . (H h)
|
\just\equiv{ def. functor H (3*) }
|
     F (g . h) + G (g . h) = (F g + G g) . (F h + G h)
|
\just\equiv{ def. functor F (3*), def. functor G (3*) }
|
     id + (g . h) = (id + g) . (id + h)
|
\just\equiv{ natural-|id|, functor-|+| }
|
     (id . id) + (g . h) = (id . id) + (g . h)
|
\qed
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.5\textwidth}
\begin{eqnarray*}
\start
|
     H id = id
|
\just\equiv{ def. functor H }
|
     F id + G id = id
|
\just\equiv{ def. functor F, def. functor G }
|
     id + id = id
|
\just\equiv{ functor-|id|-|+| }
|
     id = id
|
\qed
\end{eqnarray*}
\end{minipage}



\begin{minipage}{0.5\textwidth}
\begin{eqnarray*}
\start
|
     K (g . h) = (K g) . (K h)
|
\just\equiv{ def. functor K }
|
     G (g . h) >< F (g . h) = (G g >< F g) . (G h >< F h)
|
\just\equiv{ def. functor F (3*), def. functor G (3*) }
|
     (g . h) >< id = (g >< id) . (h >< id)
|
\just\equiv{ natural-|id|, functor-|><| }
|
     (g . h) >< (id . id) = (g . h) >< (id . id)
|
\qed
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.5\textwidth}
\begin{eqnarray*}
\start
|
     K id = id
|
\just\equiv{ def. functor K }
|
     G id >< F id = id
|
\just\equiv{ def. functor F, def. functor G }
|
     id >< id = id
|
\just\equiv{ functor-|id|-|><| }
|
     id = id
|
\qed
\end{eqnarray*}
\end{minipage}


\noindent {\large \textbf{Exercício 8}}

\begin{minipage}{0.5\textwidth}
\begin{eqnarray*}
\start
|
     H id = id
|
\just\equiv{ def. functor H }
|
     (F . G) id = id
|
\just\equiv{ composição de functores }
|
     F (G id) = id
|
\just\equiv{ functor-id-G }
|
     F id = id
|
\just\equiv{ functor-id-F }
|
     id = id
|
\qed
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.5\textwidth}
\begin{eqnarray*}
\start
|
     H (f . g) = (H f) . (H g)
|
\just\equiv{ def. functor H }
|
     (F . G) (f . g) = ((F . G) f) . ((F . G) g)
|
\just\equiv{ composição de functores }
|
     F (G (f . g)) = (F (G f)) . (F (G g))
|
\just\equiv{ Functor-F, Functor-G }
|
     F (G f . G g) = F (G f . G g)
|
\qed
\end{eqnarray*}
\end{minipage}



\noindent {\large \textbf{Exercício 9}}

\end{document}
%----------------- Fim do documento -------------------------------------------%
