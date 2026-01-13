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
%format (fac (n)) = "{" n "!}"
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


\title{Cálculo de Programas \\ Resolução - Ficha 03}
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
     assocl . assocr = id
|
\just\equiv{ Def. |assocl|, fusão-|><|, reflexão-|><|, eq-|><| }
|
lcbr(
     (id >< p1) . assocr = p1
)(
     p2 . p2 . assocr = p2
)
|
\just\equiv{ def-|><|, universal-|><| }
|
lcbr(
     lcbr(
          p1 . assocr = p1 . p1
     )(
          p1 . p2 . assocr = p2 . p1
     )
)(
     p2 . p2 . assocr = p2
)
|
\just\equiv{ associação à direita }
|
lcbr(
     p1 . assocr = p1 . p1
)(
     lcbr(
          p1 . p2 . assocr = p2 . p1
     )(
          p2 . p2 . assocr = p2
     )
)
|
\just\equiv{ universal-|><| }
|
lcbr(
     p1 . assocr = p1 . p1
)(
     p2 . assocr = split (p2 . p1) p2
)
|
\just\equiv{ natural-|id|, universal-|><|}
|
     assocr = split (p1 . p1) (p2 >< id)
|
\end{eqnarray*}

\noindent {\large \textbf{Exercício 3}}

\begin{minipage}{0.3\textwidth}
\begin{eqnarray*}
\start
\frac{
     \begin{array}{c}
          |f : A -> B| \\
          |g : C -> D|
     \end{array}
}{
     |f >< g : A >< C -> B >< D|
}
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.3\textwidth}
\begin{eqnarray*}
\start
\frac{
     \begin{array}{c}
          |f : A -> B| \\
          |g : A -> C|
     \end{array}
}{
     |split f g : A -> B >< C|
}
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.3\textwidth}
\begin{eqnarray*}
\start
\frac{
     \begin{array}{c}
          |f : A -> B| \\
          |g : B -> C|
     \end{array}
}{
     |f . g : A -> C|
}
\end{eqnarray*}
\end{minipage}

\begin{minipage}{0.45\textwidth}
\begin{eqnarray*}
\start
\frac{
     \begin{array}{c}
          |p2 : A >< B -> B| \\
          |p1 : A >< B -> A|
     \end{array}
}{
     |split p2 p1 : A >< B -> B >< A|
}
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.45\textwidth}
\begin{eqnarray*}
\start
\frac{
     \begin{array}{c}
          |id : A -> A| \\
          |swap : B >< C -> C >< B|
     \end{array}
}{
     |id >< swap : A >< (B >< C) : A >< (C >< B)|
}
\end{eqnarray*}
\end{minipage}


\begin{eqnarray*}
\start
\frac{
     \begin{array}{c}
          |swap : D >< E -> E >< D| \\
          |id >< swap : A >< (B >< C) : A >< (C >< B)|
     \end{array}
}{
     |swap . (id >< swap) : A >< (B >< C) -> (C >< B) >< A|
}
\end{eqnarray*}

\begin{eqnarray*}
\start
|
(F0) : (g >< f) . swap = swap . (f >< g)
|
\end{eqnarray*}
\begin{eqnarray*}
\start
|
     beta . (f >< (g >< h))
|
\just\equiv{ Def. |beta| }
|
     swap . (id >< swap) . (f >< (g >< h))
|
\just\equiv{ (F0) }
|
     (id >< swap) . swap . (f >< (g >< h))
|
\just\equiv{ (F0) }
|
     (swap >< id) .((g >< h) >< f) . swap
|
\just\equiv{ functor-|><| }
|
     ((swap . (g >< h)) >< (id . f)) . swap
|
\just\equiv{ (F0) }
|
     (((h >< g) . swap) >< (f . id)) . swap
|
\just\equiv{ functor-|><| }
|
     ((h >< g) >< f) . (swap >< id) . swap
|
\just\equiv{ (F0) }
|
     ((h >< g) >< f) . swap (id >< swap)
|
\just\equiv{ Def. |beta| }
|
     ((h >< g) >< f) . beta
|
\end{eqnarray*}


\noindent {\large \textbf{Exercício 4}}
\begin{eqnarray*}
\start
| const k x = const k (x) = const k (id x) = const k . id = const k = k |
\end{eqnarray*}

\noindent {\large \textbf{Exercício 6}}

\begin{minipage}{0.5\textwidth}
\begin{eqnarray*}
\start
\frac{
     \begin{array}{c}
          |const False : A -> Bool| \\
          |id : A -> A|
     \end{array}
}{
     |split (const False) id : A -> Bool >< A|
}
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.5\textwidth}
\begin{eqnarray*}
\start
\frac{
     \begin{array}{c}
          |f : A -> C| \\
          |g : B -> C|
     \end{array}
}{
     |either f g : A + B -> C|
}
\end{eqnarray*}
\end{minipage}

\begin{eqnarray*}
\start
\frac{
     \begin{array}{c}
          |split (const False) id : A -> Bool >< A| \\
          |split (const True) id : A -> Bool >< A|
     \end{array}
}{
     |either (split (const False) id) (split (const True) id) : A + A -> Bool >< A|
}
\end{eqnarray*}


\noindent {\large \textbf{Exercício 7}}
\begin{eqnarray*}
\start
|
     alfa = either (split (const False) id) (split (const True) id)
|
\just\equiv{ universal-|+| }
|
lcbr(
     alfa . i1 = split (const False) id
)(
     alfa . i2 = split (const True) id
)
|
\just\equiv{ pointwise }
|
lcbr(
     (alfa . i1) . a = split (const False) id a
)(
     (alfa . i2) . a = split (const True) id a
)
|
\just\equiv{ Def. composição, Def. split }
|
lcbr(
     alfa (i1 a) = (const False a, id a)
)(
     alfa (i2 a) = (const True a, id a)
)
|
\just\equiv{ Def. const, Def. id }
|
lcbr(
     alfa (i1 a) = (False, a)
)(
     alfa (i2 a) = (True, a)
)
|
\end{eqnarray*}

\noindent {\large \textbf{Exercício 8}}

\begin{minipage}{0.5\textwidth}
\begin{eqnarray*}
\start
\frac{
     \begin{array}{c}
          |p1 : A >< B -> A| \\
          |id : C -> C|
     \end{array}
}{
     |p1 >< id : (A >< B) >< C -> A >< C|
}
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.5\textwidth}
\begin{eqnarray*}
\start
\frac{
     \begin{array}{c}
          |p2 : A >< B -> B| \\
          |p1 : (A >< B) >< C -> A >< B|
     \end{array}
}{
     |p2 . p1 : (A >< B) >< C -> B|
}
\end{eqnarray*}
\end{minipage}

\begin{eqnarray*}
\start
\frac{
     \begin{array}{c}
          |p1 >< id : (A >< B) >< C -> A >< C| \\
          |p2 . p1 : (A >< B) >< C -> B|
     \end{array}
}{
     |split (p1 >< id) (p2 . p1) : (A >< B) >< C -> (A >< C) >< B|
}
\end{eqnarray*}


\begin{eqnarray*}
\start
|
     xr . split (split f g) h = split (split f h) g
|
\just\equiv{ universal-|><| }
|
lcbr(
     p1 . xr . split (split f g) h = split f h
)(
     p2 . xr . split (split f g) h = g
)
|
\just\equiv{ Def. |xr|, cancelamento-|><| }
|
lcbr(
     (p1 >< id) . split (split f g) h = split f h
)(
     p2 . p1 . split (split f g) h = g
)

|
\just\equiv{ absorção-|><|, cancelamento-|><| }
|
lcbr(
     split (p1 . split f g) (id . h) = split f h
)(
     p2 . split f g = g
)
|
\just\equiv{ cancelamento-|><| }
|
lcbr(
     split f h = split f h
)(
     g = g
)
|
\qed
\end{eqnarray*}

\noindent {\large \textbf{Exercício 9}}

\begin{code}

type Key = String
type Aut = String
type Pag = Int
type Bib = [(Key, [Aut])]
type Aux = [(Pag, [Key])]
type Ind = [(Aut, [Pag])]

mkInd :: (Bib, Aux) -> Ind
mkInd = undefined
\end{code}


\end{document}
%----------------- Fim do documento -------------------------------------------%
