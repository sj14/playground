-module(tail).
-export([fib/3,fib/1,perfect/1, fibP/1]).

fib(0,P,_C) ->
    P;
fib(N,P,C) ->
    fib(N-1,C,P+C).




perfect(N,N,S) ->
    N==S;
perfect(N,M,S) when N rem M == 0 ->
    perfect(N,M+1,S+M);
perfect(N,M,S) ->
    perfect(N,M+1,S).

perfect(N) ->
    perfect(N,1,0).

fibP(0) ->
     {0,1};
fibP(N) ->
     {P,C} = fibP(N-1),
    {C,P+C}.

fib(N) ->
    {P,_} = fibP(N),
    P.
