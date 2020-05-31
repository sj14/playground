-module(hof).

-export([doubleAll/1,
         doubleAll_test/0,
         evens/1,
         evens_test/0,
         product/1,
         product_test/0]).

% doubleAll([]) -> [];
% doubleAll([X|Xs]) ->
%     [ 2*X | doubleAll(Xs) ].

doubleAll([]) -> [];
doubleAll(X) -> lists:map(fun (Y) -> 2 * Y end, X).

doubleAll_test() ->
    [2, 14, 20] = doubleAll([1, 7, 10]),
    [] = doubleAll([]),
    pass.

% evens([]) -> [];
% evens([X|Xs]) when X rem 2 == 0 ->
%     [X | evens(Xs) ];
% evens([_|Xs]) ->
%     evens(Xs).

evens(X) -> lists:filter(fun (Y) -> Y rem 2 == 0 end, X).

evens_test() ->
    [10, 4] = evens([1, 7, 10, 4]),
    [] = evens([]),
    pass.

% product([]) -> 1;
% product([X|Xs]) -> X * product(Xs).

product(X) -> lists:foldr(fun (Y, Z) -> Y * Z end, 1, X).

product_test() ->
    30 = product([5, 3, 2]),
    pass.
