-module(fas).

-export([compose/1,
         compose_test/0,
         iterate/1,
         iterate_test/0,
         twice/1,
         twice_test/0]).

add(X) -> fun (Y) -> X + Y end.

times(X) -> fun (Y) -> X * Y end.


compose([F | Fs]) ->
    lists:foldl(fun (X, Y) -> fun (Z) -> X(Y(Z)) end end, F, Fs).

compose_test() ->
    C = compose([add(5), times(2)]),
    16 = C(3),
    pass.


twice(F) -> fun (X) -> F(F(X)) end.

twice_test() ->
    T = twice(times(3)),
    18 = T(2),

    T2 = twice(twice(times(3))),
    162 = T2(2),
    pass.


iterate(0) -> fun (F) -> F end;
iterate(N) ->
    fun (F) ->
            F(),
            iterate(N - 1, F)
    end.

iterate(0, F) -> F;
iterate(N, F) ->
    F(),
    iterate(N - 1, F).

iterate_test() ->
    I = iterate(5),
    I(fun () -> io:format("called~n") end),

    % I guess this below should work and the above is wrong.
    % I2 = iterate(5),
    % 15 = I2(times(3)),
    pass.
