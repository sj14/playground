-module(super).
-export([super/0]).

super() ->
    process_flag(trap_exit, true),
    E = spawn_link(echo,listener,[]),
    register(echo,E),
    io:format("echo spawned.~n"),
    T = spawn_link(talk,worker,[]),
    register(talk,T),
    io:format("worked spawned as Pid ~w.~n",[whereis(talk)]),
    loop(E,T).

loop(E,T) ->
     receive
        {'EXIT', T, _} -> 
            NewT = spawn_link(talk,worker,[]),
            register(talk,NewT),
            io:format("worked re-spawned as Pid ~w.~n",[whereis(talk)]),
            loop(E,NewT);
         {'EXIT', E, _} -> 
            timer:sleep(1000), 
            NewE = spawn_link(echo,listener,[]),
            register(echo,NewE),
            io:format("echo re-spawned.~n"),
            loop(NewE,T)
    end.




    

