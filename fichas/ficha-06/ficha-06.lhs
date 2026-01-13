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


\title{Cálculo de Programas \\ Resolução - Ficha 06}
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

main = undefined
\end{code}
%endif


\maketitle


\noindent {\large \textbf{Exercício 1}}
\begin{eqnarray*}
\start
|
     ap . (curry f >< id) = f
|
\just\equiv{ pointwise }
|
     ap . (curry f >< id) (a,b) = f (a,b)
|
\just\equiv{ def. comp, def-|><| }
|
     curry f a b = f (a,b)
|
\just\equiv{ def. curry }
|
     f (a,b) = f (a,b)
|
\qed
\end{eqnarray*}


\noindent {\large \textbf{Exercício 2}}
\begin{eqnarray*}
\start
|
     ap . (curry f >< id) = f
|
\just\equiv{ pointwise }
|
     ap . (curry f >< id) (a,b) = f (a,b)
|
\just\equiv{ def. comp, def-|><| }
|
     curry f a b = f (a,b)
|
\just\equiv{ |f = uncurry g| }
|
     curry (uncurry g) a b = uncurry g (a,b)
|
\just\equiv{ def. curry }
|
     uncurry g (a,b) = uncurry g (a,b)
|
\qed
\end{eqnarray*}


\noindent {\large \textbf{Exercício 3}}
\begin{eqnarray*}
\start
|
     curry (f . (g >< h)) = curry (ap . (id >< h)) . curry f . g
|
\just\equiv{ universal-exp }
|
     f . (g >< h) = ap . ((curry (ap . (id >< h)) . curry f . g) >< id)
|
\just\equiv{ natural-|id|, functor-|><| }
|
     f . (g >< h) = ap . (curry (ap . (id >< h)) >< id) . (curry f . g >< id)
|
\just\equiv{ cancelamento-exp }
|
     f . (g >< h) = ap . (id >< h) . ((curry f . g) >< id)
|
\just\equiv{ functor-|><| }
|
     f . (g >< h) = ap . ((curry f . g) >< h)
|
\just\equiv{ natural-|id|, functor-|><| }
|
     f . (g >< h) = ap . (curry f >< id) . (g >< h)
|
\just\equiv{ cancelamento-exp }
|
     f . (g >< h) = f . (g >< h)
|
\qed
\end{eqnarray*}

\noindent {\large \textbf{Exercício 4}}

\begin{minipage}{0.4\textwidth}
\begin{eqnarray*}
\start
|
     flip (flip f) = f
|
\just\equiv{ def. |flip| }
|
     curry (uncurry (flip f) . swap) = f
|
\just\equiv{ pointwise }
|
     curry (uncurry (flip f) . swap) a b = f a b
|
\just\equiv{ def. curry }
|
     (uncurry (flip f) . swap) (a,b) = f a b
|
\just\equiv{ def. comp, def. |swap| }
|
     uncurry (flip f) (b,a) = f a b
|
\just\equiv{ def. uncurry }
|
     flip f b a = f a b
|
\just\equiv{ ... }
|
     ...
|
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.5\textwidth}
\begin{eqnarray*}
\start
|
     flip f x y = f y x
|
\just\equiv{ def. |flip| }
|
     curry (uncurry f . swap) x y = f y x
|
\just\equiv{ def. curry }
|
     (uncurry f . swap) (x,y) = f y x
|
\just\equiv{ def. comp, def. |swap| }
|
     uncurry f (y,x) = f y x
|
\just\equiv{ def. uncurry }
|
     f y x = f y x
|
\qed
\end{eqnarray*}
\end{minipage}

\newpage

\noindent {\large \textbf{Exercício 5}}

\begin{minipage}{0.4\textwidth}
\begin{eqnarray*}
\start
|
     junc . unjunc = id
|
\just\equiv{ pointwise }
|
     (junc . unjunc) l = id k
|
\just\equiv{ def. |id|, def. comp, def. |unjunc| }
|
     junc (k . i1, k . i2) = k
|
\just\equiv{ def. |junc| }
|
     either (k . i1) (k . i2) = k
|
\just\equiv{ fusão-|+| }
|
     k . either i1 i2 = k
|
\just\equiv{ reflexão-|+|, natural-|id| }
|
     k = k
|
\qed
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.5\textwidth}
\begin{eqnarray*}
\start
|
     unjunc . junc = id
|
\just\equiv{ pointwise }
|
     (unjunc . junc) (f,g) = id (f,g)
|
\just\equiv{ def. |id|, def. comp, def. |junc| }
|
     unjunc either f g = (f, g)
|
\just\equiv{ def. |unjunc| }
|
     (either f g . i1, either f g . i2) = (f, g)
|
\just\equiv{ cancelamento-|+| }
|
     (f, g) = (f, g)
|
\qed
\end{eqnarray*}
\end{minipage}

\noindent {\large \textbf{Exercício 6}}
\begin{eqnarray*}
\start
|
     (for b i) . inNat = either g1 g2 . (id + for b i)
|
\just\equiv{ def. |inNat|, fusão-|+|, absorção-|+| }
|
     either (for b i . const 0) (for b i . succ) = either (g1 . id) (g2 . for b i)
|
\just\equiv{ natural-|id|, eq-|+| }
|
lcbr(
     for b i . const 0 = g1
)(
     for b i . succ = g2 . for b i
)
|
\just\equiv{ pointwise, def. comp, def. |const 0|, def. succ }
|
lcbr(
     for b i 0 = g1 ()
)(
     for b i (n+1) = g2 (for b i n)
)
|
\just\equiv{ (F8) }
|
lcbr(
     g1 = i
)(
     g2 (for b i n) = b (for b i n)
)
|
\just\equiv{ igualdade extensional (73) }
|
lcbr(
     g1 = i
)(
     g2 = b
)
|
\end{eqnarray*}


\noindent {\large \textbf{Exercício 8}}

\begin{minipage}{0.4\textwidth}
\begin{eqnarray*}
\start
|
lcbr(
     for b i 0 = i
)(
     for b i (n+1) = b (for b i n)
)
|
\just\equiv{ pointfree }
|
lcbr(
     for b i . const 0 = const i
)(
     for b i . succ = b . for b i
)
|
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.5\textwidth}
\begin{eqnarray*}
\start
|
lcbr(
     a + 0 = a
)(
     a + (n+1) = 1 + (a + n)
)
|
\just\equiv{ pointfree }
|
lcbr(
     (a+) . const 0 = const a
)(
     (a+) . succ = succ . (a+)
)
|
\end{eqnarray*}
\end{minipage}

\noindent Deduzimos então que |(a+) = for succ a|.

\newpage

\noindent {\large \textbf{Exercício 9}}
\begin{minted}{c}
int k(int n, int a) {
     int r = 0;
     int j;
     for (j = 1; j < n + 1; j++) {
          r = a + r;
     }
     return r;
};
\end{minted}

\noindent {\large \textbf{Exercício 10}}
\begin{code}
func b = (maybe b id .) . flip lookup

a = [(140999000, "Manuel"), (200100300, "Mary"), (000111222, "Teresa")]

b = [(140999000, "PT"), (200100300, "UK")]

c = [(140999000, "Braga"), (200100300, "Porto"), (151999000, "Lisbon")]
\end{code}


\end{document}
%----------------- Fim do documento -------------------------------------------%
