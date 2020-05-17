-module(list_fun).
-export([double/1, double_test/0, evens/1, evens_test/0]).

double([]) -> [];
double(X) -> double(X, []).

double([], R) -> R;
double([X|Xs], R) -> double(Xs, R ++ [X*2]).

double_test() ->
    [2,4,6] = double([1,2,3]),
    [10,12,6] = double([5,6,3]),
    pass.


evens([]) -> [];
evens(X) -> evens(X, []).

evens([], R) -> R;
evens([X|Xs], R) when X rem 2 == 0 -> evens(Xs, R ++ [X]);
evens([_X|Xs], R) -> evens(Xs, R).

evens_test() ->
    [2,8] = evens([1,2,3,7,8]),
    [4,46] = evens([93,27,4,46]),
    pass.