%% Implement the keep and discard operation on collections. 
%% Given a collection and a predicate on the collection's elements, 
%% keep returns a new collection containing those elements where 
%% the predicate is true, while discard returns a new collection 
%% containing those elements where the predicate is false.

-module(strain).
-export([keep/2, keep_opt/2, discard/2]).

keep(_, []) -> 
    {error, "empty list"};
keep(Fn, [H]) -> 
    case Fn(H) of
       	true  -> [H];
	false -> []
    end;
keep(Fn, [H|T]) -> 
    case Fn(H) of 
	true  -> [H | keep(Fn, T)];
        false -> keep(Fn, T)
    end.

keep_opt(_,[]) ->
	{error, "empty list"};
keep_opt(Fn, L) ->
	[X || X <- L, Fn(X)].


discard(_,[]) ->
	{error, "empty list"};
discard(Fn, L) ->
	[X || X <- L, not Fn(X)].
