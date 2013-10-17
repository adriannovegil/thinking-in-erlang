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
