-module(super_own).

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

    L = spawn(?MODULE, loop, [E, T]),
    register(super, L).

loop(E, T) ->
    receive
        {'EXIT', E, Reason} ->
            io:format("Echo ~w exited with reason: ~w~n. Restarting",
                      [E, Reason]),
            RE = spawn_link(echo, listener, []),
            register(echo, RE),
            loop(E, RE);
        {'EXIT', T, Reason} ->
            io:format("Talk ~w exited with reason: ~w~n. Restarting",
                      [T, Reason]),
            RT = spawn_link(talk, worker, []),
            register(talk, RT),
            loop(E, RT);
        {'EXIT', Pid, Reason} ->
            io:format("~w exited with reason: ~w~n.",
                      [Pid, Reason]),
            loop(E, T)
    end.
