%% Create a custom set type.
%% 
%% Sometimes it is necessary to define a custom data structure 
%% of some type, like a set. In this exercise you will define 
%% your own set. How it works internally doesn't matter, as long
%% as it behaves like a set of unique elements.

-module(custom_set).
-export([add/2, remove/2, in/2]).

add([],E) -> [E];
add(S,E)  -> add(S,E,S).

add([],E,S)                -> [E|S];
add([H|T],E,S) when H /= E -> add(T,E,S);
add([H|_],E,S) when H == E -> S.

in([],_)    -> false;
in([E|_],E) -> true;
in([_|T],E) -> in(T,E).

remove(S,E) -> [X || X <- S, X =/= E].

	   
