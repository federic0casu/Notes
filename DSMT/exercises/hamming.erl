%% Calculate the Hamming Distance between two DNA strands.
%%
%% Example:
%% 	GAGCCTACTAACGGGAT
%% 	CATCGTAATGACGGCCT
%% 	^ ^ ^  ^ ^    ^^
%%
%% Implementation notes:
%% The Hamming distance is only defined for sequences of equal
%% length, so an attempt to calculate it between sequences of 
%% different lengths should not work.
%%
%% If the sequences are not of equal length, return a tuple of 
%% {error, badarg}. Otherwise, return only the integer.

-module(hamming).
-export([hamming/2]).

hamming(L,R) when length(L) =/= length(R) ->
	{error, badarg};
hamming(L,R) -> 
	hamming(L,R,0).

hamming([],[],N) ->
	N;
hamming([H|TL], [H|TR], N) ->
	hamming(TL, TR, N);
hamming([_|TL], [_|TR], N) ->
	hamming(TL, TR, N+1).
