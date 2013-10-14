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
