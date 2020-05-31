-module(myzip).

-export([zip/2,
         zip_d/2,
         zip_d_test/0,
         zip_test/0,
         zip_with/3,
         zip_with_c/3,
         zip_with_c_test/0,
         zip_with_test/0]).

% a)
zip([], _) -> [];
zip(_, []) -> [];
zip([L1 | L1s], [L2 | L2s]) ->
    [{L1, L2}] ++ zip(L1s, L2s).

zip_test() ->
    [{1, 2}, {3, 4}] = zip([1, 3, 5, 7], [2, 4]),
    pass.

% b)
zip_with(_, [], _) -> [];
zip_with(_, _, []) -> [];
zip_with(F, [L1 | L1s], [L2 | L2s]) ->
    [F(L1, L2)] ++ zip_with(F, L1s, L2s).

zip_with_test() ->
    [3, 7] = zip_with(fun (X, Y) -> X + Y end,
                      [1, 3, 5, 7],
                      [2, 4]),
    pass.

% c)
zip_with_c(F, L1, L2) ->
    lists:map(fun ({X, Y}) -> F(X, Y) end, zip(L1, L2)).

zip_with_c_test() ->
    [3, 7] = zip_with_c(fun (X, Y) -> X + Y end,
                        [1, 3, 5, 7],
                        [2, 4]),
    pass.

% d)
zip_d(L1s, L2s) ->
    zip_with_c(fun (L1, L2) -> {L1, L2} end, L1s, L2s).

zip_d_test() ->
    [{1, 2}, {3, 4}] = zip_d([1, 3, 5, 7], [2, 4]),
    pass.
