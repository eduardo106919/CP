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
%format t1 = "t_1 "
%format t2 = "t_2 "
%format t3 = "t_3 "
%format t4 = "t_4 "
%format t5 = "t_5 "
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

main = undefined
\end{code}
%endif


\maketitle


\noindent {\large \textbf{Exercício 1}}
\begin{spec}
length [] = 0
length (x:xs) = 1 + length xs

reverse [] = []
reverse (x:xs) = reverse xs ++ [x]
\end{spec}

\noindent {\large \textbf{Exercício 2}}
\begin{eqnarray*}
\start
|
     take m (take n x) = take (min m n) x
|
\end{eqnarray*}

\noindent {\large \textbf{Exercício 3}}
\begin{spec}
map _ [] = []
map f (x:xs) = f x : map f xs

filter _ [] = []
filter f (x:xs) = if f x then x : filter f xs else filter f xs

uncurry f (x, y) = f x y

curry f x y = f (x, y)

flip f x y = f y x
\end{spec}

\noindent {\large \textbf{Exercício 4}}
\begin{eqnarray*}
\start
|
     (f . g) x = f (g x) = 2 * (x+1) = 2*x + 2
|
\end{eqnarray*}
\begin{eqnarray*}
\start
|
     (f . g) x = f (g x) = succ (2 * x) = 2 * x + 1
|
\end{eqnarray*}
\begin{eqnarray*}
\start
|
     (f . g) x = f (g x) = succ (length x) = length x + 1
|
\end{eqnarray*}
\begin{eqnarray*}
\start
|
     (f . g) (x,y) = f (g (x,y)) = (succ . (2*)) (x + y) = succ (2*(x+y)) = 2*x + 2*y + 1
|
\end{eqnarray*}

\noindent {\large \textbf{Exercício 5}}
\begin{eqnarray*}
\start
|
     (f . g) . h = f . (g . h)
|
\just\equiv{ pointwise }
|
     ((f . g) . h) x = (f . (g . h)) x
|
\just\equiv{ Def. comp }
|
     (f . g) (h x) = f ((g . h) x)
|
\just\equiv{ Def. comp }
|
     f (g (h x)) = f (g (h x))
|
\end{eqnarray*}


\noindent {\large \textbf{Exercício 6}}
\begin{eqnarray*}
\start
|
lcbr(
     (f . id) x = f (id x) = f x
)(
     (id . f) x = id (f x) = f x
)
|
\end{eqnarray*}

\noindent {\large \textbf{Exercício 7}}

\noindent \textbf{alínea a)}
\begin{eqnarray*}
\start
|
     store 7 [1..10]
|
\just\equiv{ (F2) }
|
     (take 10 . nub . (7:)) [1..10]
|
\just\equiv{ (F1) }
|
     take 10 (nub (7 : [1..10]))
|
\just\equiv{ Def. (|7:|) }
|
     take 10 (nub [7,1,2,3,4,5,6,7,8,9,10])
|
\just\equiv{ Def. |nub| }
|
     take 10 [7,1,2,3,4,5,6,8,9,10]
|
\just\equiv{ Def. |take| }
|
     [7,1,2,3,4,5,6,8,9,10]
|
\end{eqnarray*}

\noindent \textbf{alínea b)} o requisito (a) é cumprido mas os restantes não, pois a primeira operação a ser feita é a remoção de duplicados. Esta operação deve ser feita após adicionar |c|. O requisito (b) não é cumprido, pois primeiro seleciona-se os 10 primeiros elementos e depois adiciona-se |c|, logo a lista final terá no máximo 11 elementos, violando o requisito (c).

\noindent \textbf{alínea c)} o requisito (c) é violado, pois primeiro retira-se os primeiros 10 elementos e adiciona-se |c|, logo a lista final terá no máximo 11 elementos (caso a função |nub| não efetue mudanças na lista).

\noindent {\large \textbf{Exercício 8}}
\noindent O resultado será |["Mary","Manuel","Tia Irene","Augusto"]|. Neste exemplo é evidenciado o facto das funções en Haskell só receberem um argumento. Ao analisar a composição de funções em (F2):
\begin{itemize}
     \item |(c:) :: [a] -> [a]|
     \item |nub :: [a] -> [a]|
     \item |take 10 :: [a] -> [a]|
\end{itemize}

\noindent verificamos que a função |store| não recebe dois argumentos, recebe um argumento |c| e devolve uma função do tipo |store c :: [a] -> [a]|. Isto acontece porque Haskell aplica \textbf{currying} automaticamente.


\end{document}
%----------------- Fim do documento -------------------------------------------%
