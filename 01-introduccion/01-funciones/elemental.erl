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
