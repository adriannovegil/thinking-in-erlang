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
