-module(recursion).

-compile([export_all]).

%% 01 - Potencia de exponente natural.
%% -----------------------------------------------------------------------------
%% Definir por recursión la función
%%
%% potencia :: int -> int -> int
%%
%% tal que (potencia x n) es x elevado al número natural n. Por ejemplo,
%%
%% potencia 2 3 == 8

potencia(X, 1) -> X;
potencia(X, N) ->  X * potencia(X, N - 1).

%% 02 - Replicación de un elemento.
%% -----------------------------------------------------------------------------
%% Definir por recursión la función
%% 
%% replicate2 :: int -> a -> [a]
%%
%% tal que (replicate2 n x) es la lista formmada por n copias del elemento x. 
%% Por ejemplo,
%%
%% replicate2 3 2 == [2,2,2]

replicate2(1, X) -> [X];
replicate2(N, X) -> [X] ++ replicate2(N - 1, X).

%% 03 - Doble factorial.
%% -----------------------------------------------------------------------------
%% El doble factorial de un número n se define por
%%
%% 0!! = 1
%% 1!! = 1
%% n!! = n*(n-2) * ... * 3 * 1, si n es impar
%% n!! = n*(n-2) * ... * 4 * 2, si n es par
%%
%% Por ejemplo,
%%
%% 8!! = 8*6*4*2 = 384
%% 9!! = 9*7*5*3*1 = 945
%%
%% Definir, por recursión, la función
%%
%% dobleFactorial :: Int -> Int
%%
%% tal que (dobleFactorial n) es el doble factorial de n. Por ejemplo,
%%
%% sobleFactorial 8 == 384
%% sobleFactorial 9 == 945

dobleFactorial(0) -> 1;
dobleFactorial(1) -> 1;
dobleFactorial(N) -> N * dobleFactorial(N - 2).

%% 04 - Algoritmo de Euclides del máximo común divisor.
%% -----------------------------------------------------------------------------
%% Dados dos números naturales, a y b, es posible calcular su máximo común
%% divisor mediante el Algoritmo de Euclide. Este algoritmo se puede resumir en
%% la siguiente fórmula:
%%
%%			  | a, 					si b = 0
%% mcd(a,b) = |
%%			  | mcd(b, a módulo b), si b > 0
%%
%% Definir la función
%%
%% mcd :: int -> int -> int
%%
%% tal que (mcd a b) es el máximo común divisor de a y b calculado mediante el 
%% algoritmo de Euclides. Por ejemplo,
%%
%% mcd 30 45 == 15

mcd(A,0) -> A;
mcd(A,B) -> mcd(B, A rem B).
