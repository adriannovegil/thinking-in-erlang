-module('comprensionrecursion').

-compile([export_all]).

%% 1 - Suma de los cuadrados de los primeros números
%% ------------------------------------------------------------------------------------
%% Definir, por recursión; la función
%%
%% sumaCuadradosR :: int -> int
%%
%% tal que (sumaCuadradosR n) es la suma de los cuadrados de los números de 1 a n. Por
%% ejemplo,
%%
%% sumaCuadradosR 4 == 30

sumaCuadradosR(0) -> 0;
sumaCuadradosR(N) -> math:pow(N, 2) + sumaCuadradosR(N - 1).

%% Definir por comprensión, la> función
%%
%% sumaCuadradosC :: int -> int
%%
%% tal que (sumaCuadradosC n) es la suma de los cuadrados de los números de 1 a n. Por
%% ejemplo,
%%
%% sumaCuadradosC 4 == 30

sumaCuadradosC(N) ->
	lists:sum([math:pow(X, 2) || X <- lists:seq(1,N)]).

%% 2 - Número de bloques de escaleras triangulares
%% ------------------------------------------------------------------------------------
%% Se quiere formar una escalera con bloques cuadrados, de forma que tengan un número
%% determinado de escalones. Por ejemplo, una escalera con tres escalones tendría la 
%% siguiente forma:
%%
%%     XX
%%   XXXX
%% XXXXXX
%%
%% Definir, por recursión, la función
%%
%% numeroBloquesR :: int -> int
%%
%% tal que (numeroBloquesR n) es el número de bloques necesarios para contruir una 
%% escalera con n escalone. Por ejemplo,
%%
%% numeroBloquesR 1 == 2
%% numeroBloquesR 3 == 12
%% numeroBloquesR 10 == 110

numeroBloquesR(0) -> 0;
numeroBloquesR(N) -> 2 * N + numeroBloquesR(N - 1).

%% Definir, por comprensión, la función
%%
%% numeroBloquesC :: int -> int
%%
%% tal que (numeroBloquesC n) es el número de bloques necesarios para construir una 
%% escalera con n escalones. Por ejemplo,
%%
%% numeroBloquesC == 2
%% numeroBloquesC 3 == 12
%% numeroBloquesC 10 == 110

numeroBloquesC(N) ->
	lists:sum([2 * X || X <- lists:seq(1,N)]).

%% 3 - Suma de los cuadrados de los impares entre los primeros números
%% ------------------------------------------------------------------------------------
%% Definir, por recursión, la función
%%
%% sumaCuadradosImparesR :: int -> int
%%
%% tal que (sumaCuadradosImparesR n) es la suma de los cuadrados de ĺos números impares
%% desde 1 hasta n. Por ejemplo,
%%
%% sumaCuadradosImparesR 1 == 1
%% sumaCuadradosImparesR 7 == 84
%% sumaCuadradosImparesR 4 == 10

sumaCuadradosImparesR(1) -> 1;
sumaCuadradosImparesR(N) ->
	if
		N rem 2 /= 0 ->
			math:pow(N,2) + sumaCuadradosImparesR(N-1);
		true ->
		 	sumaCuadradosImparesR(N-1)
	end.

%% Definir, por comprensión, la función
%%
%% sumaCuadradosImparesC :: int -> int
%%
%% tal que (sumaCuadradosImparesC n) es la suma de los cuadrados de los números impares
%% desde 1 hasta n. Por ejemplo,
%%
%% sumaCuadradosImparesC 1 == 1
%% sumaCuadradosImparesC 7 == 84
%% sumaCuadradosImparesC 4 == 19

sumaCuadradosImparesC(N) ->
	lists:sum([math:pow(X,2) || X <- lists:seq(1,N), X rem 2 /= 0]).

%% Otra definición más simples es

sumaCuadradosImparesC2(N) ->
	lists:sum([math:pow(X,2) || X <- lists:seq(1,N,2)]).

%% 4 - Operaciones con los dígitos de los números.
%% ------------------------------------------------------------------------------------

%% 4.1 Lista de los dígitos de un número

%% 4.1.1 - Definir por recursión, la función 
%%
%% digitosR :: int -> [int]
%%
%% tal que (digitosR n) es la lista de los dígitos del número n. Por ejemplo,
%%
%% digitosR 320274 == [3,2,0,2,7,4]

digitosR_helper(0) -> [];
digitosR_helper(N) ->
	[N rem 10] ++ digitosR_helper(N div 10).

digitosR(N) ->
	lists:reverse(digitosR_helper(N)).

%% 4.1.2 - Definir, por comprensión, la función
%%
%% digitosC :: int -> [int]
%%
%% tal que (digitosC n) es la lista de los dígitos del número n. Por ejemplo,
%%
%% digitosC 320274 == [3,2,0,2,7,4]
%%
%% Indicaćión: Usar las funciones integer_to_list y list_to_integer.

digitosC(N) ->
	[ list_to_integer([X]) || X <- integer_to_list(N)].

%% 4.2 - Suma de los dígitos de un número

%% 4.2.1 - Definir, por recursión, la función
%%
%% sumaDigitosR :: int -> int
%%
%% tal que (sumaDigitosR n) es la suma de los dígitos de n. Por ejemplo,
%%
%% sumaDigitosR 3 = 3
%% sumaDigitosR 2454 == 15
%% sumaDigitosR 20045 == 11

sumaDigitosR(0) -> 0;
sumaDigitosR(N) ->
	(N rem 10) + sumaDigitosR(N div 10).

%% 4.2.2 - Definir, sin usar recursión, la función
%%
%% sumaDigitosNR :: int -> int
%%
%% tal que (sumaDigitosNR n) es la suma de los dígitos de n. Por ejemplo,
%%
%% sumaDigitosNR 3 = 3
%% sumaDigitosNR 2454 == 15
%% sumaDigitosNR 20045 == 11

sumaDigitosNR(N) ->
	lists:sum(digitosR(N)).

%% 4.3 - Definir la función
%%
%% esDigito :: int -> int -> bool
%%
%% tal que (esDigito x n) se verifica si x es un dígito de n. Por ejemplo,
%%
%% esDigito 4 1041 == true
%% esDigito 3 1041 == false

esDigito(X, N) ->
	lists:member(X,digitosR(N)).

%% 4.4 - Definir la función
%%
%% numeroDeDigitos :: int -> int
%%
%% tal que (numeroDeDigitos x) es el número de dígitos de x. Por ejemplo,
%%
%% numeroDeDigitos 34047 == 5

numeroDeDigitos(X) ->
	length(digitosR(X)).

%% 4.5 - Número correspondiente a una lista de dígitos

%% 4.5.1 - Definir, por recursión, la función
%%
%% listaNumeroR :: [int] -> int
%%
%% tal que (loistaNumeroR xs) es el número formado por los dígitos de la lista
%% xs. Por ejemplo,
%%
%% listaNumeroR [5] == 5
%% listaNumeroR [1,3,4,7] == 1347
%% listaNumeroR [0,0,1] == 1

listaNumeroR(X) ->
	listaNumeroRHelper(lists:reverse(X)).

listaNumeroRHelper([X]) -> X;
listaNumeroRHelper([H|T]) -> H + 10 * listaNumeroRHelper(T).

%% 4.5.2 - Definir, por comprensión, la función
%%
%% listaNumeroC :: [int] -> int
%%
%% tal que (listaNumeroC xs) es el número formado por los dígitos de la lista
%% xs. Por ejemplo,
%%
%% listaNumeroC [5] == 5
%% listaNumeroC [1,3,4,7] == 1347
%% listaNumeroC [0,0,1] == 1

listaNumeroC(X) ->
	lists:sum([Y * math:pow(10,N) || {Y,N} <- lists:zip(lists:reverse(X),lists:seq(0,length(X)-1))]).

%% 4.6 - Concatenación de dos números

%% 4.6.1 - Definir, por recursión, la función
%%
%% pegaNumerosR :: int -> int -> int
%%
%% tal que (pegaNumerosR x y) es el número resultante de "pegar" los números
%% x e y. Por ejemplo,
%%
%% pegaNumerosR 12 987 == 12987
%% pegaNumerosR 1204 7 == 12047
%% pegaNumerosR 100 100 == 100100

pegaNumerosR(X,Y) ->
	if
		Y < 10 ->
			10 * X + Y;
		true ->
			10 * pegaNumerosR(X, (Y div 10)) + (Y rem 10)
	end.

%% 4.6.2 - Definir, sin usar recursión, la función
%%
%% pegaNumerosNR :: int -> int -> int
%%
%% tal que (pegaNumerosNR x y) es el número resultante de "pegar" los números
%% x e y. Por ejemplo,
%%
%% pegaNumerosNR 12 987 == 12987
%% pegaNumerosNR 1204 7 == 12047
%% pegaNumerosNR 100 100 == 100100

pegaNumerosNR(X,Y) ->
	listaNumeroC(digitosR(X) ++ digitosR(Y)).

%% 4.7 - Primer dígito de un número

%% 4.7.1 - Definir, por recursión, la función
%%
%% primerDigitoR :: int -> int
%%
%% tal que (primerDigitoR n) es el primer dígito de n. Por ejemplo,
%%
%% primerDigitoR 425 == 4

primerDigitoR(N) ->
	if
		N < 10 ->
			N;
		true ->
			primerDigitoR(N div 10)
	end.

%% 4.7.2 - Definir, sin usar recursión, la función
%%
%% primerDigitoNR :: int -> int
%% 
%% tal que (primerDigitoNR n) es el primer dígito de n. Por ejemplo,
%%
%% primerDigitoNR 425 == 4

primerDigitoNR(N) ->
	[H|_] = digitosR(N),
	H.

%% 4.8 - Definir la función
%%
%% ultimoDigito :: int -> int
%%
%% tal que (ultimoDigito n) es el último dígito de n. Por ejemplo,
%%
%% ultimoDigito 425 == 5

ultimoDigito(N) ->
	N rem 10.

%% 4.9 - Número con los dígitos invertidos

%% 4.9.1 - Definir la función
%%
%% inverso:: int -> int
%%
%% tal que (inverso n) es el número obtnido escribiendo los dígitos de n en 
%% orden inverso. Por ejemplo,
%%
%% inverso 42578 == 87524
%% inverso 203 == 302

inverso(N) ->
	listaNumeroC(lists:reverse(digitosR(N))).

%% 4.9.2 - Definir, usando show y read, la función
%%
%% inverso2 :: int -> int
%%
%% tal que (inverso2 n) es el número obtenido escribiendo los dígitos de n en
%% orden inverso. Por ejemplo,
%%
%% inverso2 42578 == 87524
%% inverso2 203 == 302

inverso2(N) ->
	list_to_integer(lists:reverse(integer_to_list(N))).

%% 4.10 - Definir la función
%%
%% capicua:: int -> bool
%%
%% tal que (capicua n) se verifica si los dígitos de n son los mismos de 
%% izquierda a derecha que de derecha a izquierda. Por ejemplo,
%%
%% capicua 1234 == false
%% capicua 1221 == true
%% capicua 4 == true

capicua(N) ->
	N == inverso(N).

%% 4.11 - Suma de los dígitos de 2^1000
%% En el problema 16 del proyecto Euler se pide calcular la suma de los 
%% dígitos de 2^1000. Lo resolveremos mediante los distintos apartados de este
%% ejercicio.

%% 4.11.1 - Definir la función
%%
%% euler16 :: int -> int
%%
%% tal que (euler16 n) es la suma de los dígitos de 2^n. Por ejemplo,
%%
%% euler16 4 == 7

euler16(N) ->
	sumaDigitosNR(trunc(math:pow(2,N))).

%% 4.11.2 - Calcular la suma de los dígitos de 2^1000

%% solución: euler16 1000 == 1366

%% 4.12 - En el enunciado de uno de los problemas de las Olimpiadas matemáticas
%% de Brasil se define el primitivo de un número como sigue:
%%
%% Dado un número natural n, multiplicamos todos sus dígitos, repetimos este
%% procedimiento hasta que quede un solo dígitos al cual llamamos primitivo de 
%% n. Por ejemplo para 327: 3 x 2 x 7 = 42 y 4 x 2 = 8. Por los tanto, el 
%% primitivo de 327 es 8.
%%
%% Definir la función
%%
%% primitivo :: int -> int
%%
%% tal que (primitivo n) es el primitivo de n. Por ejemplo
%%
%% primitivo 327 == 8

producto(0) -> 1;
producto(N) -> (N rem 10) * producto(N div 10).
	
primitivo(N) ->
	if
		N < 10 ->
			N;
		true ->
			primitivo(producto(N))
	end.

%% 4.13 - Dos números son equivalentes si la media de sus dígitos son iguales.
%% Por ejemplo, 3205 y 41 son equivalentes ya que
%%
%% 3 + 2 + 0 + 5   4 + 1
%% ------------- = -----
%%       4           2
%%
%% Definir la función
%% 
%% equivalentes :: int -> int -> bool
%%
%% tal que (equivalentes x y) se verifica si los números x e y son 
%% equivalentes. Por ejemplo,
%%
%% equivalentes 3205 41 == true
%% equivalentes 3205 25 == false

media(L) ->
	lists:sum(L) / length(L).

equivalentes(X,Y) ->
	media(digitosC(X)) == media(digitosC(Y)).

%% 4.14 - Un número x es especial si el número de ocurrencia de cada dígito d
%% de x en x^2 es el doble del número de ocurrencia de d en x. Por ejemplo, 
%% 72576 es especial por que tiene un 2, un 5, un 6 y dos 7 y su cuadrado es
%% 5267275776 que tiene exactamente dos 2, dos 5, dos 6 y cuatro 7.
%%
%% Definir la función
%%
%% especial :: int -> bool
%%
%% tal que (especial x) se verifica si x es un número espacial. Por ejemplo,
%%
%% especial 72576 == true
%% especial 12 == false

especial(X) ->
	lists:sort(lists:append(digitosR(X), digitosR(X))) == 
	lists:sort(digitosR(trunc(math:pow(X,2)))).

%% Calcula el menor número especial mayor que 72576

calculoHelper(Init, Fin) -> 

	Result = [X || X <- lists:seq(Init,Fin), especial(X)],

	if
		length(Result) > 0 ->
			lists:nth(1, Result);		
		true ->
			calculoHelper(Fin + 1, Fin + 100)
	end.

calculo() ->
	Inicio = 72577,
	calculoHelper(Inicio, Inicio + 10).
	
%% 5 - Cuadrados de los elementos de una lista.
%% ------------------------------------------------------------------------------------

%% 5.1 - Definir, por comprensión, la función
%%
%% cuadradosC :: [int] -> [int]
%%
%% tal que (cuadradosC xs) es la lista de los cuadrados de xs. Por ejemplo,
%%
%% cuadradosC [1,2,3] == [1,4,9]

cuadradosC(Xs) ->
	[math:pow(X,2) || X <- Xs].

%% 5.2 - Definir, por recursión, la función
%%
%% cuadradosR :: [int] -> [int]
%%
%% tal que (cuadradosR xs) es la lista de los cuadrados de xs. Por ejemplo,
%%
%% cuadrados [1,2,3] == [1,4,9]

cuadradosR([X]) -> [math:pow(X,2)];
cuadradosR([H|T]) -> [math:pow(H,2)] ++ cuadradosR(T).

%% 6 - Números impares de una lista
%% ------------------------------------------------------------------------------------

%% 6.1 - Definir, por comprensión, la función
%%
%% imparesC :: [int] -> [int]
%%
%% tal que (imparesC xs) es la lista de los números impares de xs. Por ejemplo,
%%
%% imparesC [1,2,4,3,6] == [1,3]

imparesC(Xs) ->
	[X || X <- Xs, X rem 2 =/= 0].

%% 6.2 - Definir, por recursión, la función
%%
%% imparesR :: [int] -> [int]
%%
%% tal que (imparesR xs) es la lista de los números impares de xs. Por ejemplo,
%%
%% imparesR [1,2,4,3,6] == [1,3]

imparesR([]) -> [];
imparesR([H|T]) -> 
	if
		H rem 2 =/= 0 ->
			[H] ++ imparesR(T);
		true ->
			imparesR(T)
	end.

%% 7 - Cuadrados de los elementos impares
%% ------------------------------------------------------------------------------------

%% 7.1 - Definir, por comprensión, la función
%%
%% imparesCuadradosC :: [int] -> [int]
%%
%% tal que (imparesCuadradosC xs) es la lista de los cuadrados de los números impares
%% de xs. Por ejemplo, 
%%
%% imparesCuadradosC [1,2,4,3,6] == [1,9]

imparesCuadradosC(Xs) ->
	[X*X || X <- Xs, X rem 2 =/= 0].

%% 7.2 - Definir, por recursión, la función
%%
%% imparesCuadradosR :: [int] -> [int]
%%
%% tal que (imparesCuadradosR xs) es la lista de los cuadrados de los números impares
%% de xs. Por ejemplo,
%%
%% imparesCuadradosR [1,2,4,3,6] == [1,9]

imparesCuadradosR([]) -> [];
imparesCuadradosR([H|T]) ->
	if
		H rem 2 =/= 0 ->
			[math:pow(H,2)] ++ imparesCuadradosR(T);
		true ->
			imparesCuadradosR(T)
	end.

%% 8 - Suma de los cuadrados de los elementos impares
%% ------------------------------------------------------------------------------------

%% 8.1 - Definir, por comprensión, la función
%%
%% sumaCuadradosImparesC :: [int] -> int
%%
%% tal que (sumaCuadradosImparesC xs) es la suma de los cuadrados de los números 
%% impares de la lista xs. Por ejemplo, 
%%
%% sumaCuadradosImparesC [1,2,4,3,6] == 10

sumaCuadradosImparesC3(Xs) ->
	lists:sum([X*X || X <- Xs, X rem 2 =/= 0]).

%% 8.2 - Definir, por recursión, la función
%%
%% sumaCuadradosImparesR :: [int] -> int
%%
%% tal que (sumaCuadradosImparesR xs) es la suma de los cuadrados de los números 
%% impares de la lista xs. Por ejemplo,
%%
%% sumaCuadradosImparesR [1,2,4,3,6] = 10

sumaCuadradosImparesR2([]) -> 0;
sumaCuadradosImparesR2([H|T]) ->
	if
		H rem 2 =/= 0 ->
			H*H + sumaCuadradosImparesR2(T);
		true ->
			sumaCuadradosImparesR2(T)
	end.
