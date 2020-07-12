-module(super).

-export([loop/2, super/0]).

super() ->
    process_flag(trap_exit, true),
    E = spawn_link(echo, listener, []),
    register(echo, E),
    io:format("echo spawned.~n"),
    T = spawn_link(talk, worker, []),
    register(talk, T),
    io:format("worked spawned as Pid ~w.~n",
              [whereis(talk)]),
    % L = spawn_link(?MODULE, loop, [E, T]),
    % register(super, L).
    loop(E, T).

loop(E, T) ->
    receive
        {'EXIT', T, _} ->
            NewT = spawn_link(talk, worker, []),
            register(talk, NewT),
            io:format("worked re-spawned as Pid ~w.~n",
                      [whereis(talk)]),
            loop(E, NewT);
            % spawn_link(?MODULE, loop, [E, NewT]);
        {'EXIT', E, _} ->
            % timer:sleep(1000),
            %
            % adding the timer causes the rerror message blow because the echo server
            % was not started in time when the worker tries to send the message to echo.
            %
            % =ERROR REPORT==== 12-Jul-2020::15:30:22.528443 ===
            % Error in process <0.124.0> with exit value:
            % {badarg,[{talk,work,1,[{file,"talk.erl"},{line,15}]}]}
            %
            NewE = spawn_link(echo, listener, []),
            register(echo, NewE),
            io:format("echo re-spawned.~n"),
            loop(NewE, T);
            % spawn_link(?MODULE, loop, [NewE, T]);
        _ ->
            io:format("received unknown message.~n"),
            loop(E, T)
    end.
