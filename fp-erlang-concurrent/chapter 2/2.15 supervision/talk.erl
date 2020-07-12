-module(talk).
-export([worker/0]).

worker() ->
    work(0).

work(N) ->
    Msg = {self(),N},
    echo!Msg,
    io:format("~w sent.~n",[Msg]),
    receive
        _Reply -> 
            timer:sleep(500),
            work(N+1)
    end.

