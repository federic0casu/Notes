%% The Collatz Conjecture or 3x+1 problem can be summarized as follows:
%%
%% Take any positive integer n. If n is even, divide n by 2 to 
%% get n / 2. If n is odd, multiply n by 3 and add 1 to get 3n + 1. 
%% Repeat the process indefinitely. 
%%
%% The conjecture states that no matter which number you start with, 
%% you will always reach 1 eventually.
%%
%% Given a number n, return the number of steps required to reach 1.

-module(collatz).
-export([collatz/1]).

collatz(N) when is_integer(N) ->
	collatz(N, 0);
collatz(_) ->
	{error, "not an integer"}.

collatz(1,T)                     -> T;
collatz(N,T) when (N rem 2) == 0 -> collatz(N div 2,T+1);
collatz(N,T) when (N rem 2) == 1 -> collatz(3*N+1,T+1).
