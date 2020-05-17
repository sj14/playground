% chapter 2.15

-module(mylists).
-export([product/1, product_test/0, product_direct/1, product_direct_test/0, maximum/1, maximum_test/0, maximum_direct/1, maximum_direct_test/0]).

% tail recursive
product(Xs) -> product(Xs, 1).

product([], P) ->  P;
product([X|Xs], P) ->  product(Xs, X*P).

product_test() ->
    6 = product([1,2,3]),
    27 = product([3,3,3]),
    pass.

% direct
product_direct([]) -> 1;
product_direct([X|Xs]) -> product_direct(Xs) * X.

product_direct_test() ->
    6 = product_direct([1,2,3]),
    27 = product_direct([3,3,3]),
    pass.


% tail
maximum(Xs) -> maximum(Xs, 0).
maximum([], M) -> M;
maximum([X|Xs], M) -> maximum(Xs, max(X,M)).

maximum_test() ->
    9 = maximum([5,9,3]),
    pass.

% direct
maximum_direct([X|Xs]) -> max(X,maximum_direct(Xs)).

maximum_direct_test() ->
    9 = maximum([5,9,3]),
    pass.
