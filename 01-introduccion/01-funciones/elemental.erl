-module(elemental).

-compile([export_all]).

%% 01 - Media de 3 números.
%% -----------------------------------------------------------------------------
%% Definir la función media3 tal que (media3 x y z) es la media aritmética de 
%% los números x, y y z. Por ejemplo,
%%
%% media3 1 3 8 == 4.0
%% media3 (-1) 0 7 == 2.0
%% media3 (-3) 0 3 == 0.0

media3(X, Y, Z) -> 
	(X + Y + Z) / 3.

%% 02 - Suma de euros de una colección de monedas.
%% -----------------------------------------------------------------------------
%% Definir la función sumaMonedas tal que (sumaMonedas a b c d e) es la suma de 
%% los euros correspondientes a a monedas de 1 euro, b de 2 euros, c de 5 euros,
%% d de 10 euros y e de 20 euros. Por ejemplo,
%%
%% sumaMonedas 0 0 0 0 1 == 20
%% sumaMonedas 0 0 8 0 3 == 100
%% sumaMonedas 1 1 1 1 1 == 38

sumaMonedas(A, B, C, D, E) ->
	1 * A + 2 * B + 5 * C + 10 * D + 20 * E.

%% 03 - Volumen de una esfera.
%% -----------------------------------------------------------------------------
%% Definir la función volumenEsfera tal que (volumenEsfera r) es el volumen de 
%% la esfera de radio r. Por ejemplo,
%%
%% volumenEsfera 10 == 4188.790204786391

volumenEsfera(R) ->
	(4 / 3) * math:pi() * math:pow(R, 3).

%% 04 - Área de una corona circular.
%% -----------------------------------------------------------------------------
%% definir la función areaDeCoronaCircular tal que (areaDeCoronaCircular r1 r2) 
%% es el área de una corona circular de radio interior r1 y radio exterior r2. 
%% Por ejemplo,
%%
%% areaDeCoronaCircular 1 2 == 9.42477796076938
%% areaDeCoronaCiruclar 2 5 == 65.97344572538566
%% areaDeCoronaCircular 3 5 == 50.26548245743669

areaDeCoronaCircular(R1, R2) ->
	math:pi() * (math:pow(R2, 2) - math:pow(R1, 2)).

%% 05 - Última ficra de un número.
%% -----------------------------------------------------------------------------
%% Definirla la función ultimaCifra tal que (ultimaCifra x) es la última cifra 
%% del número x. Por ejemplo,
%%
%% ultimaCifra 325 == 5

ultimaCifra(X) ->
	X rem 10.

%% 06 - Máximo de 3 elementos.
%% -----------------------------------------------------------------------------
%% Definir la función maxTres tal que (maxTres x y z) es el máximo de x, y y z. 
%% Por ejemplo,
%%
%% maxTres 6 2 4 == 6
%% maxTres 6 7 4 == 7
%% maxTres 6 7 9 == 9

maxTres(X, Y, Z) ->
	max(X, max(Y, Z)).

%% 07 - Disyunción excluyente.
%% -----------------------------------------------------------------------------
%% La disyunción excluyente xor de dos fórmulas se verifica si una es verdadera 
%% y la otra es falsa.

%% 7.1 - Definir la función xor1 que calcule la fisyunción excluyente a partir 
%% de la tabla de verdad. Usar 4 ecuaciones, una por cada línea de la tabla.

xor1(true, true) ->
	flase;
xor1(false, true) ->
	true;
xor1(true, false) ->
	true;
xor1(false, false) ->
	flase.

%% 7.2 - Definir la función xor2 que calcule la disyunción excluyente a partir 
%% de la tabla de verdad y patrone. Usar 2 ecuaciones, una por cada valor del 
%% primer argumento.

xor2(true, Y) ->
	not Y;
xor2(false, Y) ->
	Y.

%% 7.3 - Definir la función xor3 que calcule la disyunción ecluyente a partir 
%% de la disyunción (||), conjunción (&&) y negación (not). Usar 1 ecuación.

xor3 (X, Y) ->
	(X or Y) and not (X and Y).

%% 7.4 - Definir la función xor4 que calcule la disyunción excluyente a partir
%% de la desigualdad (/=). Usar 1 ecuación.

xor4 (X, Y) ->
	X =/= Y.

%% 08 - Rotación de listas.
%% -----------------------------------------------------------------------------

%% 8.1 - Definir la función rota1 tal que (rota1 xs) es la lista obtenida 
%% poniendo el primer elemento de xs al final de la lista. Por ejemplo,
%%
%% rota1 [2,5,7,3] == [2,5,7,3]

rota([H|T]) ->
	T ++ [H].

%% 8.2 definir la función rota tal que (rota n xs) es la lista obtenida 
%% poniendo los n primeros elemntos de xs al final de la lista. por ejemplo,
%%
%% rota 1 [3,2,5,7] = [2,5,7,3]
%% rota 2 [3,2,5,7] = [5,7,3,2]
%% rota 3 [3,2,5,7] = [7,3,2,5]

rota(N, L) ->
	%%{[H],[T]} = lists:split(N, L),
	%%T ++ H.
	lists:sublist(L, N + 1, length(L)) ++ lists:sublist(L, N).

%% 09 - Ranggo de una lista.
%% -----------------------------------------------------------------------------
%% Definir la función rango tal que (rango xs) es la lista formada por el menor 
%% y mayor elemento de xs. Por ejemplo,
%%
%% rango [3,2,7,5] == [2,7]
%%
%% indicación: Se pueden usar min y max

rango(L) ->
	[lists:min(L), lists:max(L)].

%% 10 - Reconocimiento de palíndromos.
%% -----------------------------------------------------------------------------
%% Definir la función palinddromo tal que (palindromo xs) se verifica si xs es 
%% un palíndromo; es decir, es lo mismo leer xs de izquierda a derecha que de 
%% derecha a izquierda. Por ejemplo,
%%
%% palindromo [3,2,5,2,3] == true
%% palindromo [3,2,5,6,2,3] == false

palindromo(L) ->
	L == lists:reverse(L).
