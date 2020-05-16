% chapter 2.5

-module(tail_recursion).
-export([fib/1, perfect/1]).
-export([perfect_test/0]).


fib(N) -> fib(N, 0, 1).
fib(0, A, _) -> A;
fib(N, A, B) -> fib(N-1, A+B, A).

perfect(0) -> false; % undefined!?
perfect(N) -> perfect(N, 1, 0, 0). % N, counter, sum, remainder
perfect(N, C, S, _) when C == N -> N == S;
perfect(N, C, S, R) when R == 0 -> perfect(N, C+1, S+C, N rem (C+1));
perfect(N, C, S, R) when R =/= 0 -> perfect(N, C+1, S, N rem (C+1)).

perfect_test() ->
    false = perfect(0),
    false = perfect(1),
    true = perfect(6),
    true = perfect(28),
    true = perfect(496),
    false = perfect(497),
    pass.