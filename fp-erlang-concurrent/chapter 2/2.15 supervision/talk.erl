-module(talk).

-export([worker/0]).

worker() -> work(1).

work(N) ->
    case N rem 5 == 0 of
        true ->
            io:format("exiting, rem 5 is 0~n"),
            exit(whereis(echo), kill); %% kill shouldn't be able to trap, but it works?!
            % exit(whereis(echo), "rem 5 is 0");
        false -> ok
    end,
    Msg = {self(), N},
    echo !
        Msg, % fails here when echo is killed by exit(whereis(echo),kill).
    io:format("~w sent.~n", [Msg]),
    receive _Reply -> timer:sleep(500) after 500 -> ok end,
    work(N + 1).
