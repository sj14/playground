-module(echo).
-export([listener/0]).


listener() ->
    receive
        {Pid,M} ->
            io:format("~w echoed.~n",[M]),
            Pid!M,
            listener()
    end.
