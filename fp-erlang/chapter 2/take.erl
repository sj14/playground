% chapter 2.20

-module(take).
-export([take/2, take_test/0]).

take(N, X) -> take(N, X, []).
take(0, _X, R) -> R;
take(_, [], R) -> R;
take(N, [X|Xs], R) -> take(N-1, Xs, R ++ [X]).

take_test() ->
    []    = take(0, "Hello"),
    "Hel" = take(3, "Hello"),
    "Hello" = take(9, "Hello"),
    pass.