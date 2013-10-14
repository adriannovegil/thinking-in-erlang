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

%% 07 - Número de pares de naturales en un círculo.
%% -----------------------------------------------------------------------------
%% Definir la función
%% 
%% circulo :: Integer -> Integer
%% 
%% tal que (circulo n) es la cantidad de pares de números naturales (x, y) que 
%% se encuentran dentro del círculo de rario n. Por ejemplo,
%% 
%% circulo 3 = 9
%% circulo 4 = 15
%% circulo 5 = 22

circulo(N) ->
	length([{X, Y} || X <- lists:seq(0, N), 
				      Y <- lists:seq(0, N), 
					  math:pow(X, 2) + math:pow(Y, 2) < math:pow(N, 2)]).

%% 08 - Aproximación del número e.
%% -----------------------------------------------------------------------------
 
%% 8.1 - Definir la función aproxE tal que (aproxE n) es la lista cuyos 
%% elementos son los términos de la sucesión (1 + 1/m )^m desde 1 hasta n. Por 
%% ejemplo,
%% 
%% aproxE 1 == [2.0]
%% aproxE 4 == [2.0, 2.25, 2.37037037037037, 2.44140625]

aproxE(N) ->
	[math:pow((1 + 1/M),M) || M <- lists:seq(1,N)].

%% 8.2 - Cual es el límite de la sucesión (1 + 1/m )^m?

%% Solución: El límite de la sucesión, cuando n tiende a infinito, es el 
%% número e.

%% 8.3 - Definir la función errorR tal que (errorAproxE x) es el menor número 
%% de términos de la sucesión (1 + 1/m )^m necesarios para obtener su límite 
%% con un error menor que x.
%% Por ejemplo,
%% 
%% errorAproxE 0.1 == 13.0
%% errorAproxE 0.01 == 135.0
%% errorAproxE 0.001 == 1359.0
%%
%% Indicación: En Erlang, e se calcula como math:exp(1).

errorAproxEHelper(X, Init, Fin) ->
	Tmp = [M || M <- lists:seq(Init, Fin), 
		abs((math:exp(1) - (math:pow(1 + 1 / M, M)))) < X],
	if
		length(Tmp) > 0 ->
			lists:nth(1, Tmp);
		true ->
			errorAproxEHelper(X, Fin + 1, Fin + 10)
	end.

errorAproxE(X) ->
	errorAproxEHelper(X, 1, 10).

%% 8.4 - El número e también se puede definir como la suma de la siere:
%%
%% 1/0! + 1/1! + 1/2! + 1/3! + ...
%%
%% Definir la función aproxE2 tal que (aproxE2 n) es la aproximación de e que
%% se obtiene sumando los términos de la serie hasta 1/n!. Por ejemplo,
%% 
%% aproxE2 10 = 2.718281801146385
%% aproxE2 100 = 2.7182818284590455

factorial(0) ->
	1;
factorial(X) ->
	X * factorial(X - 1).

aproxE2(N) ->
	1 + lists:sum([1 / factorial(K) || K <- lists:seq(1, N)]).

%% 8.5 - Definir la constante e como 2,71828459.

%% Solución: E = 2.71828459.

%% 8.6 - Definir la función errorAproxE2 tal que (errorAproxE2 x) es el menor 
%% número de términos de la serie anterior necesarios para obtener e con un 
%% error menor que x. Por ejemplo,
%%
%% errorAproxE2 0.1 == 3.0
%% errorAproxE2 0.01 == 4.0
%% errorAproxE2 0.001 == 6.0
%% errorAproxE2 0.0001 == 7.0

errorAproxE2Helper(X, Init, Fin) ->
	Tmp = [M || M <- lists:seq(Init, Fin), 
		abs((aproxE2(M) - math:exp(1))) < X],
	if
		length(Tmp) > 0 ->
			lists:nth(1, Tmp);
		true ->
			errorAproxEHelper(X, Fin + 1, Fin + 10)
	end.

errorAproxE2(X) ->
	errorAproxE2Helper(X, 1, 10).

%% 09 - Aproximación del límite.
%% -----------------------------------------------------------------------------

%% 9.1 - Definir la función aproxLimSeno tal que (aproxLimSeno n) es la lista 
%% cuyos elementos son los términos de la sucesión sen(1/m)/(1/m) desde 1 hasta 
%% n. Por ejemplo,
%% 
%% aproxLimSeno 1 = 
%% aproxLimSeno 2 = 

aproxLimSeno(N) ->
	[math:sin(1/M)/(1/M) || M <- lists:seq(1, N)].

%% 9.2 - ¿Cuál es el límite de la sucesión sen(1/m)/(1/m)?

%% Solución: El límite es 1.

%% 9.3 - Definir la función errorLimSeno tal que (errorLimSeno x) es el menor 
%% número de términos de la sucecsión sen(1/m)/(1/m) necesarios para obtener 
%% su límite con un error menor que x. Por ejemplo,
%% 
%% errorLimSeno 0.1 == 2.0
%% errorLimSeno 0.01 == 5.0
%% errorLimSeno 0.001 == 13.0
%% errorLimSeno 0.0001 == 41.0

errorLimSenoHelper(X, Init, Fin) ->
	Tmp = [M || M <- lists:seq(Init, Fin), 
		abs(1 - math:sin(1/M)/(1/M)) < X],
	if
		length(Tmp) > 0 ->
			lists:nth(1, Tmp);
		true ->
			errorLimSenoHelper(X, Fin + 1, Fin + 10)
	end.

errorLimSeno(X) ->
	errorLimSenoHelper(X, 1, 10).

%% 10 - Cálculo del número pi.
%% -----------------------------------------------------------------------------

%% 10.1 - definir la función calculaPi tal que (calculaPi n) es la aproximación
%% del número pi calculada mediante la expresión
%%
%% 4 * (1 - 1/3 + 1/5 - 1/7 + ... + (-1)^n/(2n+1))
%%
%% Por ejemplo,
%%
%% calculaPi 3 == 2.8952380952380956
%% calculaPi 300 == 3.1449149035588526

calculaPi(N) ->
	4 * lists:sum([math:pow(-1, X)/(2*X+1) || X <- lists:seq(0,N)]).

%% 10.2 - Definir la función errorPi tal que (errorPi x) es el menor número de 
%% términos de la serie
%%
%% 4 * (1 - 1/3 + 1/5 - 1/7 + ... + (-1)^n/(2n+1))
%%
%% necesarios para obtener pi con un error menor que x. Por ejemplo,
%%
%% errorPi 0.1 == 9.0
%% errorPi 0.01 == 99.0
%% errorPi 0.001 == 999.0

errorPiHelper(X, Init, Fin) ->
	Tmp = 
		[M || M <- lists:seq(Init, Fin), 		
		abs(math:pi() - calculaPi(M)) < X],
	if
		length(Tmp) > 0 ->
			lists:nth(1, Tmp);
		true ->
			errorPiHelper(X, Fin + 1, Fin + 10)
	end.

errorPi(X) ->
	errorPiHelper(X, 1, 10).

%% 11 - Ternas pitagóricas.
%% -----------------------------------------------------------------------------

%% 11.1 - Una terna (x,y,z) de enteros positivos es pitagórica si x^2+Y^2 = z^2. 
%% Usando una lista por comprensión, definir la función
%%
%% pitagoricas :: Int -> [(Int,Int,Int)]
%%
%% tal que (pitagoricas n) es la lista de todas las ternas pitagóricas cuyas 
%% componentes están entre 1 y n. Por ejemplo,
%%
%% pitagoricas 10 == [(3,4,5), (4,3,5), (6,8,10), (8,6,10)]

pitagoricas(N) ->
	[[X, Y, Z] || X <- lists:seq(1, N), 
				  Y <- lists:seq(1, N), 
				  Z <- lists:seq(1, N),
				  math:pow(X, 2) + math:pow(Y, 2) == math:pow(Z, 2)].

%% 11.2 - Definir la función
%%
%% numeroDePares :: (Int, Int, Int) ->
%%
%% tal que (numeroDePares t) es el número de elementos pares de la terna t. Por
%% ejemplo,
%%
%% numeroDePares (3,5,7) == 0
%% numeroDePares (3,6,7) == 1
%% numeroDePares (3,6,4) == 2
%% numeroDePares (4,6,4) == 3

numeroDePares([X, Y, Z]) ->
	lists:sum([1 || N <- [X, Y, Z], N rem 2 == 0]).

%% 11.3 - Definir la función
%%
%% conjetura :: int -> Bool
%%
%% tal que (conjetura n) se verifica si todas las ternas pitagóricas cuyas 
%% componentes están entre 1 y n tienen un número impar de números pares. Por
%% ejemplo,
%%
%% conjetura 10 == true

conjetura(N) -> 
	lists:foldl(fun(X, Y) -> X and Y end, true, 
			    [numeroDePares(T) rem 2 /= 0 || T <- pitagoricas(N)]).

%% 11.4 Demostrar la conjetura para todas las ternas pitagóricas.

%% Solución: Sea (x,y,z) una terna pitagórica. Entonces x^2+Y^2 = z^2. Pueden
%% darse 4 casos:
%% Caso 1: x e y son pares. Entonces x^2, y^2 y z^2 también lo son. Luego el 
%% número de componentes pares es 3 que es impar.
%% Caso 2: x es par e y es impar. Entonces, x^2 es par, y^2 es impar y z^2 es
%% impar. Luedo el número de componentes pares es 1 que es impar.
%% Caso 3: x es impar e y es par. Análogo al caso 2.
%% Caso 4: x e y son impares. Entonces x^2 e y^2 también son impares y z^2 es
%% par. Luego el número de componentes pares es 1 impar.

%% 12 - Problema 9 del Proyecto Euler.
%% -----------------------------------------------------------------------------

%% 12.1 - Una terna pitagórica es una terna de números naturales (a,b,c) tal que 
%% a < b < c y a^2 + b^2 = c^2. Por ejemplo (3,4,5) es una terna pitagórica.
%% Definir la función
%%
%% ternasPitagoricas :: int -> [[int]]
%%
%% tal que (ternasPitagoricas x) es la lista de las ternas pitagóricas cuya 
%% suma es x. Por ejemplo,
%%
%% ternasPitagoricas 12 = [(3,4,5)]
%% ternasPitagoricas 60 = [(10,24,26), (15,20,25)]

ternasPitagoricas(X) ->
	[[A,B,C] || A <- lists:seq(1, X), 
			    B <- lists:seq(A + 1, X), 
		    	C <- [X - A - B],
		    	math:pow(A, 2) + math:pow(B, 2) == math:pow(C, 2)
	].

%% 12.2 - Definir la constante euler9 tal que euler9 es producto a*b*c donde 
%% (a,b,c) es la única terna pitagórica tal que a + b + c = 1000. Calcular el 
%% valor euler9.

euler9() ->
	[H|_] = ternasPitagoricas(1000),
	lists:foldl(fun(X, Producto) -> X * Producto end, 1, H).

%% 13 - Producto escalar
%% -----------------------------------------------------------------------------
%% El producto escalar de dos listas de enteros xs e ys de longitud n viene dado
%% por la suma de los productos de los elementos correspondientes. Definir por 
%% comprensión la función
%%
%% productoEscalar :: [int] -> [int] -> int
%%
%% tal que (productoEscalar xs ys) es el producto escalar de las listas xs e ys.
%% Por ejemplo,
%%
%% productoEscalar [1,2,3] [4,5,6] == 32

productoEscalar(Xs, Ys) ->
	lists:sum([ X * Y || {X,Y} <- lists:zip(Xs, Ys)]).

%% 14 - Suma de pares de elementos consecutivos.
%% -----------------------------------------------------------------------------
%% Definir por comprensión, la función
%%
%% sumaConsecutivos :: [int] -> [int]
%%
%% tal que (sumaConsecutivos xs) es la suma de los pares de elementos 
%% consecutivos de la lista xs. Por ejemplo,
%%
%% sumaConsecutivos [3,1,5,2] == [4,6,7]
%% sumaConsecutivos [3] == []

sumaConsecutivos([H|T] = Xs) ->
	Suma = [X + Y || {X,Y} <- lists:zip(Xs, T ++ [H])],
	lists:sublist(Suma,1,length(Suma)-1).

%% 15 - Posiciones de un elemento en una lista.
%% -----------------------------------------------------------------------------
%% En el tema se ha definido la función
%%
%% posiciones :: Eq a -> a -> [a] -> [int]
%% 
%% tal que (posiciones x xs) es la lista de las posiciones ocupadas por el 
%% elemento x en la lista. Por ejemplo,
%%
%% posiciones 5 [1,5,3,5,5,7] == [1,3,4]
%%
%% Definir, usando la función busca (definida más abajo), la función
%%
%% posiciones2 :: a -> a -> [a] -> [int]
%%
%% tal que posiciones 2 sea equivalente a posiciones

%% Función (busca x ys) es la segunda componente del primer par de ys cuya
%% primera componente sea igual a x.

busca(X,L) ->
	[H|_] = [ V || {X2, V} <- L, X2 == X],
	H.

%% Modificamos la función busca para que retorne solamente la primera 
%% coincidencia, si no todas las que encuentre.

busca2(X,L) ->
	[ V || {X2, V} <- L, X2 == X].

posiciones2(X, Xs) ->
	busca2(X, lists:zip(Xs, lists:seq(1,length(Xs)))).
