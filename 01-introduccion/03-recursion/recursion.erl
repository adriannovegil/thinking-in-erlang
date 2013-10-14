-module(recursion).

-compile([export_all]).

%% 01 - Potencia de exponente natural.
%% -----------------------------------------------------------------------------
%% Definir por recursión la función
%%
%% potencia :: int -> int -> int
%%
%% tal que (potencia x n) es x elevado al número natural n. Por ejemplo,
%%
%% potencia 2 3 == 8

potencia(X, 1) -> X;
potencia(X, N) ->  X * potencia(X, N - 1).

%% 02 - Replicación de un elemento.
%% -----------------------------------------------------------------------------
%% Definir por recursión la función
%% 
%% replicate2 :: int -> a -> [a]
%%
%% tal que (replicate2 n x) es la lista formmada por n copias del elemento x. 
%% Por ejemplo,
%%
%% replicate2 3 2 == [2,2,2]

replicate2(1, X) -> [X];
replicate2(N, X) -> [X] ++ replicate2(N - 1, X).
