% chapter 2.5

-module(tail_recursion).
-export([fib/1, perfect/1]).


fib(N) -> fib(N, 0, 1).
fib(0, A, _) -> A;
fib(N, A, B) -> fib(N-1, A+B, A).


perfect(N) -> perfect(N, 1, 0, 0). % N, counter, sum, remainder
perfect(N, C, S, _) when C == N -> N == S;
perfect(N, C, S, R) when R == 0 -> perfect(N, C+1, S+C, N rem (C+1));
perfect(N, C, S, R) when R =/= 0 -> perfect(N, C+1, S, N rem (C+1)).
