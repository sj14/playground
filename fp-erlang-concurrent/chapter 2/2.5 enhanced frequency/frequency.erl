%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).

-export([init/0]).

-export([allocate/0,
         clear/0,
         deallocate/1,
         start/0,
         stop/0]).

-export([allocate_test/0]).

start() ->
    register(frequency, spawn(frequency, init, [])).

%% These are the start functions used to create and
%% initialize the server.

init() ->
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

clear() ->
    receive
        Msg ->
            io:format("discarded msg: ~w~n", [Msg]),
            clear()
        after 0 -> ok
    end.

%% Hard Coded
get_frequencies() -> [10, 11, 12, 13, 14, 15].

%% The Main Loop

loop(Frequencies) ->
    receive
        {request, Pid, allocate} ->
            % timer:sleep(100), % for testing purpose
            {NewFrequencies, Reply} = allocate(Frequencies, Pid),
            Pid ! {reply, Reply},
            loop(NewFrequencies);
        {request, Pid, {deallocate, Freq}} ->
            % timer:sleep(100), % for testing purpose
            NewFrequencies = deallocate(Frequencies, Freq),
            Pid ! {reply, ok},
            loop(NewFrequencies);
        {request, Pid, stop} ->
            % timer:sleep(100), % for testing purpose
            Pid ! {reply, stopped}
    end.

%% Functional interface

allocate() ->
    clear(),
    frequency ! {request, self(), allocate},
    receive
        {reply, Reply} -> Reply after 100 -> timeout
    end.

deallocate(Freq) ->
    clear(),
    frequency ! {request, self(), {deallocate, Freq}},
    receive
        {reply, Reply} -> Reply after 100 -> timeout
    end.

stop() ->
    clear(),
    frequency ! {request, self(), stop},
    receive
        {reply, Reply} -> Reply after 100 -> timeout
    end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq | Free], Allocated}, Pid) ->
    case lists:keymember(Pid, 2, Allocated) of
        true ->
            {{[Freq | Free], Allocated},
             {error, already_allocated}};
        _ -> {{Free, [{Freq, Pid} | Allocated]}, {ok, Freq}}
    end.

% manually added by me
allocate_test() ->
    {{[2, 4], [{1, pid123}, {3, pid1}]}, {ok, 1}} =
        allocate({[1, 2, 4], [{3, pid1}]}, pid123),
    {{[1, 2, 4], [{3, pid123}]},
     {error, already_allocated}} =
        allocate({[1, 2, 4], [{3, pid123}]}, pid123),
    ok.

deallocate({Free, Allocated}, Freq) ->
    case lists:keymember(Freq, 1, Allocated) of
        true ->
            NewAllocated = lists:keydelete(Freq, 1, Allocated),
            {[Freq | Free], NewAllocated};
        _ -> {Free, Allocated}
    end.
