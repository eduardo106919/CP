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
%==============================================================================%


\title{Cálculo de Programas \\ Resolução - Ficha 05}
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

\noindent Por |i2 . p2| inferimos |D = H|:

\begin{minipage}{0.3\textwidth}
\begin{eqnarray*}
\start
     \begin{array}{c}
          |id : A -> A| \\
          |p1 : B >< C -> B| \\
          |i2 : D -> E + D| \\
          |p2 : G >< H -> H| 
     \end{array}
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.5\textwidth}
\begin{eqnarray*}
\start
\frac{
     \frac{
          \begin{array}{c}
               |i2 : D -> E + D| \\
               |p2 : G >< H -> H|
          \end{array}
     }{
          \begin{array}{c}
               |i2 : D -> E + D| \\
               |p2 : G >< D -> D|
          \end{array}
     }
}{
     |i2 . p2 : G >< D -> E + D|
}
\end{eqnarray*}
\end{minipage}

\begin{minipage}{0.3\textwidth}
\begin{eqnarray*}
\start
\frac{
     \begin{array}{c}
          |id : A -> A| \\
          |p1 : B >< C -> B|
     \end{array}
}{
     |id + p1 : A + B >< C -> A + B|
}
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.5\textwidth}
\noindent Por |(id + p1) . i2 . p2| inferimos |A + B >< C = E + D|:
\begin{eqnarray*}
\start
\implies
|
lcbr(
     A = E
)(
     B >< C = D
)
|
\end{eqnarray*}
\end{minipage}

\begin{eqnarray*}
\start
\frac{
     \begin{array}{c}
          |id + p1 : A + B >< C -> A + B| \\
          |i2 . p2 : G >< (B >< C) -> A + B >< C|
     \end{array}
}{
     |alfa : G >< (B >< C) -> A + B|
}
\end{eqnarray*}


\noindent {\large \textbf{Exercício 2}}

\begin{minipage}{0.5\textwidth}
\begin{eqnarray*}
\start
\frac{
     \begin{array}{c}
          |join : A + A -> A| \\
          |dup : A -> A >< A|
     \end{array}
}{
     |alfa : A + A -> A >< A|
}
\end{eqnarray*}
\end{minipage}
\hfill
\begin{minipage}{0.5\textwidth}
\begin{eqnarray*}
\xymatrix@@R=4em@@C=7em{
     |A >< A|
          \ar[d]_-{|f >< g|}
&
     |A + A|
          \ar[d]^-{|f + g|}
          \ar[l]_-{|dup . join|}
\\
     |B >< B|
&
     |B + B|
          \ar[l]^-{|dup . join|}
}
\end{eqnarray*}
\end{minipage}

\noindent Propriedade grátis:
\begin{eqnarray*}
\start
|
     (f >< g) . alfa = alfa . (f + g)
|
\end{eqnarray*}


\noindent {\large \textbf{Exercício 3}}
\begin{eqnarray*}
\start
|
     nabla . (f + f) = f . nabla
|
\just\equiv{ def-|+|, fusão-|+| }
|
     either (nabla . i1 . f) (nabla . i2 . f) = f . nabla
|
\just\equiv{ universal-|+| }
|
lcbr(
     nabla . i1 . f = f . nabla . i1
)(
     nabla . i2 . f = f . nabla . i2
)
|
\just\equiv{ |nabla . i1 = id|, |nabla . i2 = id| }
|
lcbr(
     id . f = f . id
)(
     id . f = f . id
)
|
\just\equiv{ natural-|id| }
|
lcbr(
     f = f
)(
     f = f
)
|
\qed
\end{eqnarray*}

\noindent {\large \textbf{Exercício 4}}
\begin{eqnarray*}
\begin{array}{c}
     |f + g : A + B -> A' + B'| \\
     |f + g >< h : A + C >< B -> A' + C' >< B'|
\end{array}
\end{eqnarray*}
\begin{eqnarray*}
\xymatrix@@R=4em@@C=7em{
     |A + B|
          \ar[d]_-{|f + g|}
&
     |A + C >< B|
          \ar[d]^-{|f + g >< h|}
          \ar[l]_-{|alfa|}
\\
     |A' + B'|
&
     |A' + C' >< B'|
          \ar[l]^-{|alfa|}
}
\end{eqnarray*}

\noindent Podemos deduzir |alfa| como |alfa = id + p2|.

\noindent {\large \textbf{Exercício 5}}
\begin{eqnarray*}
\xymatrix@@R=4em@@C=7em{
     |(A >< B) + (A >< C)|
          \ar[d]_-{|(f >< g) + (f >< h)|}
&
     |A >< (B + C)|
          \ar[d]^-{|f >< (g + h)|}
          \ar[l]_-{|distr|}
\\
     |(A' >< B') + (A' >< C')|
&
     |A' >< (B' + C')|
          \ar[l]^-{|distr|}
}
\end{eqnarray*}

\noindent Propriedade grátis:
\begin{eqnarray*}
\start
|
     ((f >< g) + (f >< h)) . distr = distr . (f >< (g + h))
|
\end{eqnarray*}
\begin{eqnarray*}
\start
|
     h . distr . (g >< (id + f)) = k
|
\just\equiv{ propriedade grátis }
|
     h . ((g >< id) + (g >< f)) . distr = k
|
\just\equiv{ (F6) }
|
     h . ((g >< id) + (g >< f)) = k . iso(distr)
|
\just\equiv{ |iso(distr) = undistr| }
|
     h . ((g >< id) + (g >< f)) = k . undistr
|
\end{eqnarray*}

\noindent {\large \textbf{Exercício 6}}
\begin{eqnarray*}
\start
|
     (p . h) -> (f . h), (g . h)
|
\just\equiv{ def. condicional de McCarthy }
|
     either (f . h) (g . h) . (p . h)?
|
\just\equiv{ absorção-|+| }
|
     either f g . (h + h) . (p . h)?
|
\just\equiv{ natural-guarda }
|
     either f g . p? . h
|
\just\equiv{ def. condicional de McCarthy }
|
     (p -> f, g) . h
|
\end{eqnarray*}

\noindent {\large \textbf{Exercício 7}}
\begin{eqnarray*}
\start
|
     choose . parallel p f g = p -> f, g
|
\just\equiv{ def. |parallel|, def. |choose| }
|
     (p2 -> p1 . p1, p2 . p1) . split (split f g) p = p -> f, g
|
\just\equiv{ def. condicional de McCarthy }
|
     either (p1 . p1) (p2 . p1) . p2 ? . split (split f g) p = p -> f,g
|
\just\equiv{ natural-guarda }
|
     either (p1 . p1) (p2 . p1) . (split (split f g) p + split (split f g) p) . (p2 . split (split f g) p)? = p -> f, g
|
\just\equiv{ cancelamento-|><|, absorção-|+| }
|
     either (p1 . p1 . split (split f g) p) (p2 . p1 . split (split f g) p) . p ? = p -> f,g
|
\just\equiv{ cancelamento-|><| }
|
     either f g . p ? = p -> f, g
|
\just\equiv{ def. condicional de McCarthy }
|
     p -> f, g = p -> f, g
|
\qed
\end{eqnarray*}


\noindent {\large \textbf{Exercício 8}}

\noindent \textbf{Primeira} propriedade:
\begin{eqnarray*}
\start
|
     split ((p -> f, h)) ((p -> g, i))
|
\just\equiv{ def. condicional de McCarthy (twice) }
|
     split (either f h . p ?) (either g i . p ?)
|
\just\equiv{ fusão-|><| }
|
     split (either f h) (g i) . p ?
|
\just\equiv{ lei da troca }
|
     either (split f g) (split h i) . p ?
|
\just\equiv{ def. condicional de McCarthy }
|
     p -> split f g , split h i
|
\end{eqnarray*}


\noindent \textbf{Segunda} propriedade:
\begin{eqnarray*}
\start
|
     p -> split f g , split f h
|
\just\equiv{ (F11) }
|
     split ((p -> f , f)) ((p -> g , h))
|
\just\equiv{ (F9) }
|
     split f ((p -> g , h))
|
\end{eqnarray*}


\noindent \textbf{Terceira} propriedade:
\begin{eqnarray*}
\start
|
     p -> (p -> a , b) , (p -> c , d)
|
\just\equiv{ def. condicional de McCarthy }
|
     either (either a b . p ?) (either c d . p ?) . p ?
|
\just\equiv{ absorção-|+| }
|
     either (either a b) (either c d) . (p ? + p ?) . p ?
|
\just\equiv{ (F10) }
|
     either (either a b) (either c d) . (i1 + i2) . p?
|
\just\equiv{ absorção-|+|, cancelamento-|+| }
|
     either a d . p ?
|
\just\equiv{ def. condicional de McCarthy }
|
     p -> a , d
|
\end{eqnarray*}


\noindent {\large \textbf{Exercício 9}}
\begin{code}
f = undefined
\end{code}


\end{document}
%----------------- Fim do documento -------------------------------------------%
