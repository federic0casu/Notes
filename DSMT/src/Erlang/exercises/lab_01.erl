%% lab_01.erl

-module(lab_01).
-export([hello/2, rsum/1, reverse/1, take/2, mymax/1, splitter/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hello(S) ->
    io:format("Hello ~s~n", [S]).

hello(_, 0) -> ok;

hello(S, N) ->
    hello(S),
    hello(S, N-1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rsum(L) ->
    rsum(L, 0).

rsum([], S) ->
    S;

rsum([H|T], S) ->
    rsum(T, S+H).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reverse(L) ->
    reverse(L, []).

reverse([], R) -> R;    %% If the list to be reversed is empty or it has been
                        %% iterated through the end, return the reversed list R.

reverse([H|T], R) ->
    reverse(T, [H|R]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

take(L, P) ->
    take(L, P, 0).

take(_, P, _) when P < 0 ->
    io:format("Index is not a positive integer.~n"),
    done;

take([], _, C) when C == 0 ->
    io:format("Input list is empty.~n"),
    done;

take([H|_], P, C) when P == C ->
    H;

take([_|T], P, C) when C < P ->
    take(T, P, C+1);

take(_,_,_) ->
    io:format("Index out of bound.~n"),
    done.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mymax([]) ->
    io:format("Empty list.~n");

mymax([H|T]) ->
    mymax([H|T], H).

mymax([], M) -> M;

mymax([H|T], M) when M >= H ->
    mymax(T, M);

mymax([H|T], M) when M < H ->
    mymax(T, H).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

splitter([]) ->
    io:format("Empty list.~n");
splitter(L) -> 
    splitter(L, [], []).

splitter(L1, O, E) when L1 == [] -> %% END: we iterated through the whole list
    {reverse(O), reverse(E)};
splitter([H|T], O, E) when H rem 2 == 0 -> %% H is even
    splitter(T, O, [H|E]);
splitter([H|T], O, E) when H rem 2 == 1 -> %% H is odd
    splitter(T, [H|O], E).