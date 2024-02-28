-module(mylist).
-export([myreverse/1, myfind/2, mydelete/2, mydeleteV2/2, pushback/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
myreverse([]) -> 
	{badarg, "empty list"};
myreverse(L) -> myreverse(L,[]).

myreverse([],R)     -> R;
myreverse([H|T],R)  -> myreverse(T,[H|R]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
myfind([],E)    -> {not_found,E};
myfind([E|_],E) -> {found,E};
myfind([_|T],E) -> myfind(T,E).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pushback([],E) -> [E];
pushback(L,E)  -> pushback(L,E,[]).

pushback([],E,R)    -> myreverse([E|R]);
pushback([H|T],E,R) -> pushback(T,E,[H|R]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mydelete(L,E) -> [X || X <- L, X =/= E].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mydeleteV2([],_) -> [];
mydeleteV2(L,E)  -> mydeleteV2(L,E,[]).

mydeleteV2([],_,R)    -> myreverse(R);
mydeleteV2([E|T],E,R) -> myreverse([T|R]);
mydeleteV2([H|T],E,R) -> mydeleteV2(T,E,[H|R]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


