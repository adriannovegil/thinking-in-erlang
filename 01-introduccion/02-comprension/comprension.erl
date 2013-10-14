-module(comprension).

-compile([export_all]).

%% 01 - Suma de los cuadrados de los n primeros números.
%% -----------------------------------------------------------------------------
%% Definir, por comprensión, la función
%%
%% sumaDeCuadrados :: Integer -> Integer
%%
%% tal que (sumaDeCuadrados n) es la suma de los cuadrados de los primeros n 
%% números; es decir, 1^2 + 2^2 + ... + n^2. Por ejemplo,
%%
%% sumaDeCuadrados 3 = 14
%% sumaDeCuadrados 100 = 338350

sumaDeCuadrados(N) ->
	lists:sum([ math:pow(X, 2) || X <- lists:seq(1,N)]).

%% 02 - Listas con un elemento replicado.
%% -----------------------------------------------------------------------------
%% Definir por comprensión la función
%%
%% replica :: Int -> a -> [a]
%%
%% tal que (replica n x) es la lista formada por n copias del elemento x. Por 
%% ejemplo,
%%
%% replica 3 true == [true, true, true]

replica(N, X) ->
	[ X || _ <- lists:seq(1,N)].

%% 03 - Triángulos aritméticos.
%% -----------------------------------------------------------------------------

%% 3.1 - Definir la función suma tal que (suma n) es la suma de los n primeros 
%% números. Por ejemplo,
%%
%% suma 3 == 6

suma(a,N) ->
	L = lists:seq(1, N),
	F = fun(X, Suma) -> Suma + X end,
	lists:foldl(F, 0, L);
suma(b, N) ->
	(1 + N) * N div 2;
suma(c,N) ->
	lists:sum(lists:seq(1,N)).

%% 3.2 - Los triángulos aritméticos se forman somo sigue.
%%
%%  1
%%  2  3
%%  4  5  6
%%  7  8  9 10
%% 11 12 13 14 15
%% 16 16 18 19 20 21
%%
%% Definir la función línea tal que (linea n) es la línea n-ésima de los 
%% triángulos aritméticos. Por ejemplo,
%%
%% linea 4 == [7,8,9,10]
%% linea 5 == [11,12,13,14,15]

linea(N) ->
	lists:seq(suma(c, (N - 1)) + 1, suma(c, N)).

%% 3.3 - definir la función triángulo tal que (triangulo n) es el 
%% triángulo artimético de altura n. Por ejemplo,
%%
%% triangulo 3 = [[1], [2,3], [4,5,6]]
%% triangulo 4 = [[1], [2,3], [4,5,6], [7,8,9,10]]

triangulo(N) ->
	[linea(X) || X <- lists:seq(1,N)].

%% 04 - Números perfectos.
%% -----------------------------------------------------------------------------
%% Un enetero positivo es perfecto si es igual a la suma de sus factores, 
%% excluyendo el propio número. Definir por comprensión la función
%%
%% perfectos :: Int -> [Int]
%% 
%% tal que (perfectos n) es la lista de todos los números perfectos menores
%% que n. Por ejemplo,
%% 
%% perfectos 500 == [6, 28, 496]

factores(N) ->
	[X || X <- lists:seq(1, N), N rem X == 0 ].

perfectos(N) ->
	[X || X <- lists:seq(1, N), (lists:sum(factores(X) -- [X]) == X)].

%% 05 - Números abundantes.
%% -----------------------------------------------------------------------------
%% Un número natural n se denomina abundante si es menor que la suma de sus 
%% divisores propios. Por ejemplo, 12 y 30 son abundantes pero 5 y 28 no lo son.

%% 5.1 - Definir una función numeroAbundante tal que (numeroAbundante n) se 
%% verifica si n es un número abundante. Por ejemplo,
%% 
%% numeroAbundante 5 == False
%% numeroAbundante 12 == True
%% numeroAbundante 28 == False
%% numeroAbundante 30 == True

divisores(N) ->
	[X || X <- lists:seq(1, N - 1), N rem X == 0].

numeroAbundante(N) ->
	N < lists:sum(divisores(N)).

%% 5.2 - Definir una función numerosAbundantesMenores tal que 
%% (numerosAbundantesMenores n) es la lista de número abundantes menores o 
%% iguales que n. Por ejemplo,
%%
%% numerosAbundantesMenores 50 = [12,18,20,24,30,36,40,42,48]

numerosAbundantesMenores(N) ->
	[X || X <- lists:seq(1, N), numeroAbundante(X)].

%% 5.3 - Definir la función todosPares tal que (todosPares n) se verifica si 
%% todos los números abundantes menores o iguales que n son pares. Por 
%% ejemplo,
%%
%% todosPares 10 = True
%% todosPares 100 = True
%% todosPares 1000 = False

todosPares(N) ->
	lists:all( fun(X) ->
		if
			X rem 2 == 0-> true;
			true -> false
		end
	end, numerosAbundantesMenores(N)).


%% 5.4 - Definir la constante primerAbundanteImpar que calcule el primer número 
%% natural abundante impar. Determinar el valor de dicho número.

primerAbundanteImparHelper(Init, Fin) -> 
	Impares = [X || X <- lists:seq(Init,Fin), numeroAbundante(X) and ((X rem 2) /= 0)],
	if
		length(Impares) > 0 ->
			lists:nth(1, Impares);		
		true ->
			primerAbundanteImparHelper(Fin + 1, Fin + 100)
	end.

primerAbundanteImpar() -> 
	primerAbundanteImparHelper(1, 10).

%% 06 - Problema 1 del Proyecto Euler.
%% -----------------------------------------------------------------------------
%% Definir la función
%% 
%% euler1 :: Integer -> Integer
%% 
%% tal que (euler1 n) es la suma de todos los múltiplos de 3 ó 5 menores que n.
%% Por ejemplo,
%% 
%% euler1 10 == 23

multiplo(X, Y) ->
	X rem Y == 0.

euler1(N) ->
	lists:sum([X || X <- lists:seq(1, N - 1), multiplo(X, 3) or multiplo(X, 5)]).
