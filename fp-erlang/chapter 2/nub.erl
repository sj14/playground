% chapter 2.22

-module(nub).
-export([nub/1, nub_test/0]).

nub(X) -> nub(X, []).

nub([], R) -> R;
nub([X|Xs], R) -> 
    case lists:member(X,R) of
        true -> nub(Xs, R);
        false -> nub(Xs, R ++ [X])
    end.

nub_test() ->
    [1] = nub([1,1]),
    [1,5,32] = nub([1,1,5,32,5,32,5]),
    pass.