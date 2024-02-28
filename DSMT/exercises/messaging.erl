-module(messaging).
-export([server/1, client/3, mylen/1, myreverse/1, multiprocserver/0, add/0, sub/0]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mylen(L) -> mylen(L, 0).

mylen([],C)     -> C;
mylen([_|T], C) -> mylen(T, C+1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
myreverse([]) -> 
	{badarg, "empty list"};
myreverse(L) -> myreverse(L,[]).

myreverse([],R)     -> R;
myreverse([H|T],R)  -> myreverse(T,[H|R]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
server(Count) ->
	io:format("Number of requests: ~p~n", [Count]),
	receive
		{Pid, {L, len}}     -> Len = mylen(L),   Pid ! Len, server(Count+1);
		{Pid, {L, reverse}} -> R = myreverse(L), Pid ! R,   server(Count+1);
		{Pid, {_, _}}       -> Pid ! {error, "bad argument"};
		_Unexpected         -> stop
	end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
client(Pid, _, stop) ->
	Pid ! {self(), stop},
	receive
		ok         -> io:format("Server has been stopped.~n");
		{error, E} -> io:format("ERROR: ~s~n", [E]);
		_Unexpected              -> stop
	end;
client(Pid, L, len) ->
	Pid ! {self(), {L, len}},
	receive
		Len when is_integer(Len) -> io:format("len(L): ~p~n", [Len]);
		{error, E}               -> io:format("ERROR: ~s~n", [E]);
		_Unexpected              -> stop
	end;
client(Pid, {X,Y}, add) ->
	Pid ! {self(), add, X, Y},
	receive
		R when is_integer(R) -> io:format("X+Y: ~p~n", [R]);
		{error, E}               -> io:format("ERROR: ~s~n", [E]);
		_Unexpected              -> stop
	end;
client(Pid, {X,Y}, sub) ->
	Pid ! {self(), sub, X, Y},
	receive
		R when is_integer(R) -> io:format("X-Y: ~p~n", [R]);
		{error, E}               -> io:format("ERROR: ~s~n", [E]);
		_Unexpected              -> stop
	end;
client(Pid, L, reverse) when is_list(L) ->
	Pid ! {self(), {L, reverse}},
	receive
		R when is_list(R) -> io:format("reverse(L): ~p~n", [R]);
		{error, E}        -> io:format("ERROR: ~s~n", [E]);
		_Unexpected       -> stop
	end;
client(_, _, _) ->
	io:format("Usage: client(Pid, arg, cmd)       ~n"),
	io:format("arg: {X,Y} || [E1,E2,...,En])      ~n"),
	io:format("cmd: len, reverse, add, sub, stop) ~n").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
multiprocserver() ->
	S1 = spawn(?MODULE, add, []),
	S2 = spawn(?MODULE, sub, []),
	receive
		{Client, add, X, Y} -> S1 ! {Client, {X, Y}}, multiprocserver(S1, S2);
		{Client, sub, X, Y} -> S2 ! {Client, {X, Y}}, multiprocserver(S1, S2);
		{Client, stop}      -> S1 ! {self(), stop}, S2 ! {self(), stop}, Client ! ok;
		{Client, _Any}      -> Client ! {error, "bad request"}
	end.

multiprocserver(S1, S2) ->
	receive
		{Client, add, X, Y} -> S1 ! {Client, {X, Y}}, multiprocserver(S1, S2);
		{Client, sub, X, Y} -> S2 ! {Client, {X, Y}}, multiprocserver(S1, S2);
		{Client, stop}      -> S1 ! {self(), stop}, S2 ! {self(), stop}, Client ! ok;
		{Client, _Any}      -> Client ! {error, "bad request"}
	end.
	
	
add() ->
	receive
		{Client, {X, Y}} -> R = X + Y, Client ! R, add();
		{_, stop}        -> stop
	end.


sub() ->
	receive
		{Client, {X, Y}} -> R = X - Y, Client ! R, sub();
		{_, stop}        -> stop
	end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
