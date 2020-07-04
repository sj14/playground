-module(scenario).
-export([setup/0,client/2,random_elem/1]).

% Use this module to exercise the behaviour of the 
% hardened frequency server.

% Calling setup will launch the server and two clients: alice and bob.

setup() ->
    frequency_hardened:start(),
    spawn(?MODULE,client,[alice,[]]),
    spawn(?MODULE,client,[bob,[]]).

% A client, parametrised by its name (optional, but useful instrumentation),
% and the list of frequencies currently allocated to that process. Needed
% to produce calls to deallocate/1 that don't fail.

% Could also 
%   - parameterise on the ratio of allocates to deallocates
%   - deal with case when no frequencies available: here a client fails
%   - add stop commands.

client(Id,Freqs) ->
    case rand:uniform(2) of
        1 -> 
            {ok,Freq} = frequency_hardened:allocate(),
            io:format("Frequency ~w allocated to client ~w.~n", [Freq,Id]),
            timer:sleep(1000),
            client(Id,[Freq|Freqs]);
        2 ->
            Len = length(Freqs),
            case Len of 
                0 -> 
                    io:format("No frequencies to deallocate by client ~w.~n", [Id]),
                    timer:sleep(1000),
                    client(Id,Freqs);  
                _ -> 
                    Freq = lists:nth(rand:uniform(Len),Freqs),
                    frequency_hardened:deallocate(Freq), 
                    io:format("Frequency ~w deallocated by client ~w.~n", [Freq,Id]),
                    timer:sleep(1000),
                    client(Id,lists:delete(Freq,Freqs))
            end
    end.

% for debugging purposes: chooses a random element of a non-empty list.

random_elem([]) ->
    empty;
random_elem(Xs) ->
    Len = length(Xs),
    lists:nth(rand:uniform(Len),Xs).  
