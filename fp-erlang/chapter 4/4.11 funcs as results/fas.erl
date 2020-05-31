-module(fas).

-export([iterate/1,
         iterate_test/0,
         twice/1,
         twice_test/0]).

twice(F) -> fun (X) -> F(F(X)) end.

twice_test() ->
    T = twice(fun (X) -> X * 3 end),
    18 = T(2),

    T2 = twice(twice(fun (X) -> X * 3 end)),
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
    pass.
