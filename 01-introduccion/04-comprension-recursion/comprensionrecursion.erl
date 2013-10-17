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

%% Definir por comprensión, la función
%%
%% sumaCuadradosC :: int -> int
%%
%% tal que (sumaCuadradosC n) es la suma de los cuadrados de los números de 1 a n. Por
%% ejemplo,
%%
%% sumaCuadradosC 4 == 30

sumaCuadradosC(N) ->
	lists:sum([math:pow(X, 2) || X <- lists:seq(1,N)]).