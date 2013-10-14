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

%% 05 - Menor número divisible por una sucesión de números.
%% -----------------------------------------------------------------------------
%% Los siguientes ejercicios tienen como objetivo resolver el problema 5 del
%% proyecto Euler que consiste en calcular el menor número divisible por los 
%% números del 1 al 20.

%% 5.1 - Definir por recursión la función
%%
%% menorDivisible :: int -> int - int
%%
%% tal que (menorDivisible a b) es el menor número divisible por los números
%% del a al b. Por ejemplo,
%%
%% menorDivisible 2 5 == 60
%%
%% Indicación: Usar la función lcm tal que (lcm x y) es el mínimo común múltiplo
%% de x e y.

gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).
 
lcm(A,B) -> abs(A * B div gcd(A,B)).

menorDivisible(A, B) ->
	if
		A == B ->
			A;
		true ->
			lcm(A, menorDivisible(A + 1, B))
	end.

%% 5.2 - Definir la constante
%%
%% euler5 :: int
%%
%% tal que euler5 es el menor número divisible por los números del 1 al 20 y 
%% calcular su valor.

euler5() ->
	menorDivisible(1,20).

%% 06 - Número de pasos para resolver el problema de las torres de Hanoi.
%% -----------------------------------------------------------------------------
%% En un templo hindú se encuentran tres varillas de platino. En una de ellas, 
%% hay 64 anillos de oro de distintos radios, colocados de mayor a menor.
%% El trabajo de los monjes de ese templo consiste en pasarlos todos a la 
%% tercera varilla, usando la segunda varilla auxiliar, con las siguientes 
%% condiciones:
%% - En casa paso sólo se puede mover un anillo.
%% - Nunca puede haber un anillo de mayor diámetro encima de uno de menor 
%% diámetro.
%% La leyenda dice que cuando todos los anillos se encuentren en la tercera 
%% varilla, será el fín del mundo.
%% Definir la función
%%
%% numPasosHanoi :: int -> int
%%
%% tal que (numPasosHanoi n) es el número de pasos necesarios para trasladar n
%% anillos. Por ejemplo,
%%
%% numPasosHanoi 2 == 3
%% numPasosHanoi 7 == 127
%% numPasosHanoi 64 == 18446744073709551615

%% Solución: Sean A, B y C las tres varillas. La estrategia recursiva es la
%% siguiente:
%% - Caso base (n = 1): Se mueve el disco de A a C.
%% - Caso inductivo (n = m + 1): Se mueven m discos de A a C. Se mueve el disco
%% de A a B. Se mueven m discos de C a B.

numPasosHanoi(1) -> 1;
numPasosHanoi(N) -> 1 + 2 * numPasosHanoi(N - 1).
