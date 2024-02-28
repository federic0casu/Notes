-module(server).
-export([echoserver/0, queue/1, client/2]).

echoserver() ->
    receive
    	{Pid, {cmd, stop}} -> Pid ! ok;
        {Pid, ToEcho}      -> Pid ! ToEcho, echoserver();
		_Unexpected        -> {echoserver, error, "bad request"}	
    end.
    

%% Implement a server which maintains a simple FIFO queue. 
queue([]) ->
	receive
		{Pid, stop}         -> Pid ! ok;
		{Pid, {E, push}}    -> R = [E], Pid ! R, queue(R);
		{Pid, {_, pop}}     -> Pid ! [], queue([]);
		
		_Unexpected -> io:format("server:queue: unexpected msg~n")
	end;
queue([H|T]) ->
	receive
		{Pid, stop}         -> Pid ! ok;
		{Pid, {E, push}}    -> L = [H|T], R = L ++ [E], Pid ! R, queue(R);
		{Pid, {_, pop}}     -> R = T, Pid ! H, queue(R);
		
		_Unexpected -> io:format("server:queue: unexpected msg~n")
	end.
	
client(Pid, E) ->
	Pid ! {self(), E},
	receive
		Response -> io:format("Response: ~p~n", [Response])
	end.
