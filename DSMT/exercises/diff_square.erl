%% Find the difference between the square of the sum and the sum 
%% of the squares of the first N natural numbers.

-module(diff_square).
-export([diff_square/1]).

square_of_sum(N)   -> square_of_sum(N,0).
square_of_sum(0,T) -> T*T;
square_of_sum(N,T) -> square_of_sum(N-1,T+N).

sum_of_squares(N)   -> sum_of_squares(N,0).
sum_of_squares(0,T) -> T;
sum_of_squares(N,T) -> sum_of_squares(N-1,T+(N*N)).

diff_square(N) -> square_of_sum(N) - sum_of_squares(N).
