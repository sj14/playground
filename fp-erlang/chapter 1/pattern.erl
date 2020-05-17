% chapter 1.15

-module(pattern).
-export([maxThree/3,howManyEqual/3]).

maxThree(A,B,C) ->
    max(max(A, B), C).


howManyEqual(X,X,X) -> 3;
howManyEqual(X,X,_) -> 2;
howManyEqual(_,X,X) -> 2;
howManyEqual(X,_,X) -> 2;
howManyEqual(_,_,_) -> 0.
