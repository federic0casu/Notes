%% A leap year (in the Gregorian calendar) occurs:
%%
%% 1. In every year that is evenly divisible by 4.
%% 2. Unless the year is evenly divisible by 100, in which 
%%    case it's only a leap year if the year is also evenly 
%%    divisible by 400.
%%
%% Examples:
%% 	1997 was not a leap year as it's not divisible by 4.
%% 	1900 was not a leap year as it's not divisible by 400.
%% 	2000 was a leap year.

-module(leap).
-export([leap/1]).

leap(Y) when (Y rem 4) == 0 ->
	leap;
leap(Y) when (Y rem 4) /= 0 and (((Y rem 100) == 0) and ((Y rem 400) == 0)) ->
	leap;
leap(_) -> not_leap.
