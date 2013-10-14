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
