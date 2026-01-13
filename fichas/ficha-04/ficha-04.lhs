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
%==============================================================================%


\title{Cálculo de Programas \\ Resolução - Ficha 04}
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
     either (const k) (const k) = const k
|
\just\equiv{ universal-|+| }
|
lcbr(
     const k = const k . i1
)(
     const k = const k . i2
)
|
\just\equiv{ fusão-|+| }
|
lcbr(
     const k = const k
)(
     const k = const k
)
|
\qed
\end{eqnarray*}

\noindent {\large \textbf{Exercício 2}}
\begin{eqnarray*}
\start
|
     fac . either (const 0) succ = either (const 1) (mul . split succ fac)
|
\just\equiv{ universal-|+| }
|
lcbr(
     fac . either (const 0) succ . i1 = const 1
)(
     fac . either (const 0) succ . i2 = mul . split succ fac
)
|
\just\equiv{ cancelamento-|+| }
|
lcbr(
     fac . const 0 = const 1
)(
     fac . succ = mul . split succ fac
)
|
\just\equiv{ absorção-|+|, pointwise }
|
lcbr(
     const (fac 0) x = const 1 x
)(
     (fac . succ) n = (mul . split succ fac) n
)
|
\just\equiv{ def. const, def. split, def. comp }
|
lcbr(
     fac 0 = 1
)(
     fac (succ n) = mul (succ n, fac n)
)
|
\just\equiv{ def. |succ|, def. |mul| }
|
lcbr(
     fac 0 = 1
)(
     fac (n+1) = (n+1) * fac n
)
|
\end{eqnarray*}


\noindent {\large \textbf{Exercício 3}}
\begin{eqnarray*}
\start
|
     out . inNat = id
|
\just\equiv{ def. in, fusão-|+|, universal-|+| }
|
lcbr(
     out . const 0 = id . i1
)(
     out . succ = id . i2
)
|
\just\equiv{ natural-|id|, absorção-const }
|
lcbr(
     const (out 0) = i1
)(
     out . succ = i2
)
|
\just\equiv{ pointwise, def. comp, def. |succ|, def. const }
|
lcbr(
     out 0 = i1 ()
)(
     out (n+1) = i2 n
)
|
\end{eqnarray*}


\noindent {\large \textbf{Exercício 5}}
\begin{eqnarray*}
\start
|
     out . inLTree = id
|
\just\equiv{ def. in, fusão-|+|, universal-|+| }
|
lcbr(
     out . Leaf = id . i1
)(
     out . Fork = id . i2
)
|
\just\equiv{ natural-|id|, natural-|id|, def. comp }
|
lcbr(
     out (Leaf a) = i1 a
)(
     out (Fork (x,y)) = i2 (x,y)
)
|
\end{eqnarray*}
\begin{eqnarray*}
\xymatrix@@C=5em@@R=4em{
     |A|
          \ar[dr]_-{|Leaf|}
          \ar[r]^-{|i1|}
&
     |A + (LTree A >< LTree A)|
          \ar[d]^-{|inLTree = either Leaf Fork|}
&
     |LTree A >< LTree A|
          \ar[dl]^-{|Fork|}
          \ar[l]_-{|i2|}
\\
&
     |LTree A|
&
}
\end{eqnarray*}

\noindent {\large \textbf{Exercício 6}}
\begin{eqnarray*}
\start
|
     coassocl . either (id + i1, i2 . i2) = id
|
\just\equiv{ fusão-|+|, universal-|+| }
|
lcbr(
     coassocl . (id + i1) = i1
)(
     coassocl . i2 . i2 = i2
)
|
\just\equiv{ def-|+|, fusão-|+|, universal-|+| }
|
lcbr(
     lcbr(
          coassocl . i1 = i1 . i1
     )(
          coassocl . i2 . i1 = i1 . i2
     )
)(
     coassocl . i2 . i2 = i2
)
|
\just\equiv{ associação à direita }
|
lcbr(
     coassocl . i1 = i1 . i1
)(
     lcbr(
          coassocl . i2 . i1 = i1 . i2
     )(
          coassocl . i2 . i2 = i2
     )
)
|
\just\equiv{ universal-|+| }
|
lcbr(
     coassocl . i1 = i1 . i1
)(
     coassocl . i2 = either (i1 . i2) i2
)
|
\just\equiv{ universal-|+| }
|
     coassocl = either (i1 . i1) (either (i1 . i2) i2)
|
\end{eqnarray*}

\noindent {\large \textbf{Exercício 8}}
\begin{eqnarray*}
\start
|
     either (split f g) (split h k) = split (either f h) (either g k)
|
\just\equiv{ universal-|+| }
|
lcbr(
     split f g = split (either f h) (either g k) . i1
)(
     split h k = split (either f h) (either g k) . i2
)
|
\just\equiv{ fusão-|+| }
|
lcbr(
     split f g = split (either f h . i1) (either g k . i1)
)(
     split h k = split (either f h . i2) (either g k . i2)
)
|
\just\equiv{ cancelamento-|+| }
|
lcbr(
     split f g = split f g
)(
     split h k = split h k
)
|
\qed
\end{eqnarray*}

\noindent {\large \textbf{Exercício 9}}
\begin{code}
unglue = split (map getLeft . filter (isLeft . p1)) (map getRight . filter (isRight . p1))
     where getLeft (Left x, m) = (x, m)
           getRight (Right x, m) = (x, m)

glue = uncurry (++) . (map (i1 >< id) >< map (i2 >< id))
\end{code}


\end{document}
%----------------- Fim do documento -------------------------------------------%
