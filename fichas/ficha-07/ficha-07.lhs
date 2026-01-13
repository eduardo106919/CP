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
%format g1 = "g_1"
%format g2 = "g_2"
%==============================================================================%


\title{Cálculo de Programas \\ Resolução - Ficha 07}
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

\begin{minipage}{0.5\textwidth}
\begin{eqnarray*}
\xymatrix@@C=3cm@@R=1cm{
    |Seq(Nat0)|
           \ar[d]_-{|prod|}
           \ar@@/^1pc/[r]^-{|outList|}
&
    |1 + Nat0 >< Seq(Nat0)|
           \ar[d]^{|id + id >< prod|}
           \ar@@/^1pc/[l]^-{|inList|}
\\
     |Nat0|
&
     |1 + Nat0 >< Nat0|
           \ar@@/^1pc/[l]^-{|either (const 1) mul|}
}
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.5\textwidth}
\begin{code}
prod = cataList (either (const 1) mul)
\end{code}
\end{minipage}

\begin{minipage}{0.5\textwidth}
\begin{eqnarray*}
\xymatrix@@C=3cm@@R=1cm{
    |Seq(A)|
           \ar[d]_-{|reverse|}
           \ar@@/^1pc/[r]^-{|outList|}
&
    |1 + A >< Seq(A)|
           \ar[d]^{|id + id >< reverse|}
           \ar@@/^1pc/[l]^-{|inList|}
\\
     |Seq(A)|
&
     |1 + A >< Seq(A)|
           \ar@@/^1pc/[l]^-{|either nil aux|}
}
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.5\textwidth}
\begin{code}
reverse' = cataList (either nil aux)
     where aux (h,t) = t ++ [h]
\end{code}
\end{minipage}

\begin{minipage}{0.5\textwidth}
\begin{eqnarray*}
\xymatrix@@C=3cm@@R=1cm{
    |Seq(Seq(A))|
           \ar[d]_-{|concat|}
           \ar@@/^1pc/[r]^-{|outList|}
&
    |1 + Seq(A) >< Seq(Seq(A))|
           \ar[d]^{|id + id >< concat|}
           \ar@@/^1pc/[l]^-{|inList|}
\\
     |Seq(A)|
&
     |1 + Seq(A) >< Seq(A)|
           \ar@@/^1pc/[l]^-{|either nil conc|}
}
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.5\textwidth}
\begin{code}
concat' = cataList (either nil conc)
\end{code}
\end{minipage}


\begin{minipage}{0.5\textwidth}
\begin{eqnarray*}
\xymatrix@@C=3cm@@R=1cm{
    |Seq(A)|
           \ar[d]_-{|map f|}
           \ar@@/^1pc/[r]^-{|outList|}
&
    |1 + A >< Seq(A)|
           \ar[d]^{|id + id >< map f|}
           \ar@@/^1pc/[l]^-{|inList|}
\\
     |Seq(B)|
&
     |1 + A >< Seq(B)|
           \ar@@/^1pc/[l]^-{|either nil (cons . (f >< id))|}
}
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.5\textwidth}
\begin{code}
map' f = cataList (either nil (cons . (f >< id)))
\end{code}
\end{minipage}

\begin{minipage}{0.5\textwidth}
\begin{eqnarray*}
\xymatrix@@C=3cm@@R=1cm{
    |Seq(A)|
           \ar[d]_-{|maximum|}
           \ar@@/^1pc/[r]^-{|outList|}
&
    |A + A >< Seq(A)|
           \ar[d]^{|id + id >< maximum|}
           \ar@@/^1pc/[l]^-{|inList|}
\\
     |A|
&
     |A + A >< A|
           \ar@@/^1pc/[l]^-{|either id umax|}
}
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.5\textwidth}
\begin{code}
maximum' = cataList (either id umax)
\end{code}
\end{minipage}

\begin{minipage}{0.4\textwidth}
\begin{eqnarray*}
\xymatrix@@C=3cm@@R=1cm{
    |Seq(A)|
           \ar[d]_-{|filter p|}
           \ar@@/^1pc/[r]^-{|outList|}
&
    |1 + A >< Seq(A)|
           \ar[d]^{|id + id >< filter p|}
           \ar@@/^1pc/[l]^-{|inList|}
\\
     |Seq(A)|
&
     |1 + A >< Seq(A)|
           \ar@@/^1pc/[l]^-{|either nil (conc . ((Cp.cond p singl nil) >< id))|}
}
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.5\textwidth}
\begin{code}
filter' p = cataList (either nil (conc . ((Cp.cond p singl nil) >< id)))
\end{code}
\end{minipage}


\noindent {\large \textbf{Exercício 2}}
\begin{eqnarray*}
\start
|
     sumprod a = (a*) . sum
|
\just\equiv{ def. |sum|, def. |sumprod a| }
|
     cataList (either (const 0) (add . ((a*) >< id))) = (a*) . cataList (either (const 0) add)
|
\just\impliedby{ fusão-cata }
|
     (a*) . either (const 0) add = either (const 0) (add . ((a*) >< id)) . (id + id >< (a*))
|
\just\equiv{ fusão-|+|, absorção-|+| }
|
     either ((a*) . (const 0)) ((a*) . add) = either (const 0) (add . ((a*) >< id) . (id >< (a*)))
|
\just\equiv{ eq-|+| }
|
lcbr(
     (a*) . const 0 = const 0
)(
     (a*) . add = add . ((a*) >< id) . (id >< (a*))
)
|
\just\equiv{ functor-|><| }
|
lcbr(
     (a*) . const 0 = const 0
)(
     (a*) . add = add ((a*) >< (a*))
)
|
\just\equiv{ pointwise, def. |add|, def. |const 0| }
|
lcbr(
     a * 0 = 0
)(
     a * (x+y) = (a*x) + (a*y)
)
|
\end{eqnarray*}


\noindent {\large \textbf{Exercício 3}}
\begin{eqnarray*}
\start
|
     f . length = cataList (either (const 0) (2+) . p2)
|
\just\equiv{ def. |length| }
|
     f . cataList (either (const 0) (succ . p2)) = cataList (either (const 0) (2+) . p2)
|
\just\impliedby{ fusão-cata }
|
     f . either (const 0) (succ . p2) = either (const 0) ((2+) . p2) . (id + id >< f)
|
\just\equiv{ fusão-|+|, absorção-|+|, natural-|id| }
|
     either (f . const 0) (f . succ . p2) = either (const 0) ((2+) . p2 . (id >< f))
|
\just\equiv{ eq-|+|, natural-|p2| }
|
lcbr(
     f . const 0 = const 0
)(
     f . succ . p2 = (2+) . f . p2
)
|
\just\equiv{ pointwise }
|
lcbr(
     (f . const 0) n = const 0 n
)(
     (f . succ . p2) (x,y) = ((2+) . f . p2) (x,y)
)
|
\just\equiv{ def. comp, def. |p2| }
|
lcbr(
     f 0 = 0
)(
     f (y+1) = 2 + f y
)
|
\end{eqnarray*}

\noindent Dado que |2+0=2| e |2*0=0|, concluimos que |f=(2*)|.

\noindent {\large \textbf{Exercício 4}}
\begin{eqnarray*}
\start
|
     foldr (curry p2) i = f
|
\just\equiv{ def. |foldr| }
|
     cataList (either (const i) (uncurry (curry p2))) = f
|
\just\equiv{ universal-cata, |uncurry (curry f) = f| }
|
     f . inList = either (const 0) p2 . (id + id >< f)
|
\just\equiv{ def. |inList|, fusão-|+|, absorção-|+|, eq-|+| }
|
lcbr(
     f . nil = const i
)(
     f . cons = p2 . (id >< f)
)
|
\just\equiv{ pointwise }
|
lcbr(
     f [] = i
)(
     f (h:t) = f t
)
|
\end{eqnarray*}

\noindent Podemos concluir que |foldr curry p2 i| é a função constante |i|.

\noindent {\large \textbf{Exercício 5}}
\begin{eqnarray*}
\start
|
     f . (for f i) = for f (f i)
|
\just\equiv{ def. |for b i| }
|
     f . cataList (either (const i) f) = cataList (either (const (f i)) f)
|
\just\impliedby{ fusão-cata }
|
     f . either (const i) f = either (const (f i)) f . (id + f)
|
\just\equiv{ fusão-|+|, absorção-|+| }
|
     either (f . const i) (f . f) = either (const (f i) . id) (f . f)
|
\just\equiv{ eq-|+|, natural-|id|, absorção-const }
|
lcbr(
     const (f i) = const (f i)
)(
     f . f = f . f
)
|
\qed
\end{eqnarray*}


\noindent {\large \textbf{Exercício 6}}
\begin{eqnarray*}
\start
|
     for id i = for (const i) i
|
\just\equiv{ def. |for| (twice) }
|
     cataNat (either (const i) id) = cataNat (either (const i) (const i))
|
\just\equiv{ universal-cata }
|
     cataNat (either (const i) id) . inNat = either (const i) (const i) . (id + cataNat(either (const i) id))
|
\just\equiv{ cancelamento-cata, absorção-|+| }
|
     either (const i) id . (id + cataNat (either (const i) id )) = either (const i) (const i)
|
\just\equiv{ absorção-|+| }
|
     either (const i) (cataNat (either (const i) id)) = either (const i) (const i)
|
\just\equiv{ eq-|+| }
|
lcbr(
     const i = const i
)(
     cataNat (either (const i) id) = const i
)
|
\just\equiv{ universal-cata }
|
lcbr(
     true
)(
     const i . inNat = either (const i) id . (id + const i)
)
|
\just\equiv{ absorção-|+| }
|
     const i = either (const i) (const i)
|
\qed
\end{eqnarray*}

\noindent A última expressão é verdadeira, pois o resultado de |either (const i) (const i)| será sempre |i|.

\noindent {\large \textbf{Exercício 7}}
\begin{eqnarray*}
\start
\begin{array}{c}
     |const id : B -> A|^|A| \\
     |f : C -> D| \\
     |(f.) : C|^|E| |-> D|^|E|
\end{array}
\end{eqnarray*}

\noindent Temos de ter igualdade de tipos, out seja:
\begin{eqnarray*}
\start
     |A|^|A| |= D|^|E| \implies
|
lcbr(
     A = E
)(
     A = D
)
|
\end{eqnarray*}

\noindent Logo, temos:
\begin{eqnarray*}
\start
\begin{array}{c}
     |const id : B -> A|^|A| \\
     |f : C -> A| \\
     |(f.) : C|^|A| |-> A|^|A|
\end{array}
\end{eqnarray*}
\begin{eqnarray*}
\xymatrix@@C=3cm@@R=1cm{
    |Nat0|
           \ar[d]_-{|rep f|}
           \ar@@/^1pc/[r]^-{|outNat|}
&
    |1 + Nat0|
           \ar[d]^{|id + rep f|}
           \ar@@/^1pc/[l]^-{|inList|}
\\
     |A|^|A|
&
     |1 + C|^|A|
           \ar@@/^1pc/[l]^-{|either (const i) ((f.))|}
}
\end{eqnarray*}

\noindent {\large \textbf{Exercício 9}}
\begin{code}
type Date = String
type Player = String
type Game = String

db1 = [
     ("2023-10-01", ["Game1", "Game2"]),
     ("2023-10-02", ["Game2", "Game3"])
     ]

db2 = [
     ("Game1", ["PlayerA", "PlayerB"]),
     ("Game2", ["PlayerA", "PlayerC"]),
     ("Game3", ["PlayerB", "PlayerC"])
     ]

f :: [(Date, [Game])] -> [(Game, [Player])] -> [(Player, [Date])]
f = undefined

main :: IO ()
main = do
     let result = Main.f db1 db2
     print result
\end{code}


\end{document}
%----------------- Fim do documento -------------------------------------------%
